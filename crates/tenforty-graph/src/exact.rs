//! Graph JSON schema version 2: exact scalars (tenforty-yxun.3).
//!
//! Every value scalar in a graph is `{"n": "<signed integer>", "d": "<positive
//! integer>"}`, an exact rational written by the Haskell spec. In this stage the
//! runtime still computes in f64: each scalar converts once, at load, to the
//! nearest f64 (round half to even). An unbounded top bracket is a `null`
//! threshold, held in memory as `f64::INFINITY`.
//!
//! Serialization writes a finite f64 back as its exact binary value, so every
//! nonzero finite value round-trips bit for bit. Two documented exceptions:
//! - Zero is the unsigned rational 0/1, so `-0.0` (from a negative value too
//!   small for f64) reloads as `+0.0`.
//! - Saving does not recover the spec's literal: a graph loaded from `11/250`
//!   holds only the nearest double and saves that double's dyadic value
//!   (tenforty-yxun.4 keeps exact values in memory). Never regenerate exact
//!   source graphs by loading and saving them.
//!
//! A scalar that rounds beyond the f64 range is rejected; one that underflows
//! rounds to (signed) zero or a subnormal, ties to even.

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{ToPrimitive, Zero};
use serde::de::{self, Deserializer, MapAccess, Visitor};
use serde::ser::{self, SerializeMap, Serializer};
use std::fmt;

use crate::graph::ByStatus;

/// The graph JSON schema version this loader reads and writes.
pub const SCHEMA_VERSION: u32 = 2;

const EXPECTING: &str = "an exact scalar object {\"n\": \"<signed integer>\", \
     \"d\": \"<positive integer>\"} (graph JSON schema version 2)";

fn old_format<E: de::Error>(found: impl fmt::Display) -> E {
    E::custom(format!(
        "expected {EXPECTING}, found the number {found}: graph JSON written \
         before schema version 2 is no longer accepted; regenerate the graph"
    ))
}

fn is_signed_integer(text: &str) -> bool {
    let digits = text.strip_prefix('-').unwrap_or(text);
    !digits.is_empty()
        && digits.bytes().all(|b| b.is_ascii_digit())
        && (digits == "0" || !digits.starts_with('0'))
        && text != "-0"
}

fn is_positive_integer(text: &str) -> bool {
    !text.is_empty() && text.bytes().all(|b| b.is_ascii_digit()) && !text.starts_with('0')
}

/// Convert exact integer strings to the nearest f64, rejecting malformed input.
pub fn scalar_to_f64(n: &str, d: &str) -> Result<f64, String> {
    if !is_signed_integer(n) {
        return Err(format!(
            "exact scalar numerator {n:?} is not a signed integer"
        ));
    }
    if !is_positive_integer(d) {
        return Err(format!(
            "exact scalar denominator {d:?} is not a positive integer"
        ));
    }
    let numerator: BigInt = n.parse().map_err(|e| format!("{e}"))?;
    let denominator: BigInt = d.parse().map_err(|e| format!("{e}"))?;
    if numerator.is_zero() {
        return Ok(0.0);
    }
    let value = BigRational::new(numerator, denominator)
        .to_f64()
        .ok_or_else(|| format!("exact scalar {n}/{d} has no f64 value"))?;
    if !value.is_finite() {
        return Err(format!("exact scalar {n}/{d} is outside the f64 range"));
    }
    Ok(value)
}

/// The exact rational value of a finite f64, as (numerator, denominator) strings.
pub fn f64_to_scalar(value: f64) -> Result<(String, String), String> {
    if !value.is_finite() {
        return Err(format!("{value} is not finite and has no exact scalar"));
    }
    if value == 0.0 {
        return Ok(("0".into(), "1".into()));
    }
    let ratio = BigRational::from_float(value)
        .ok_or_else(|| format!("{value} has no exact rational value"))?;
    Ok((ratio.numer().to_string(), ratio.denom().to_string()))
}

struct ScalarVisitor;

impl<'de> Visitor<'de> for ScalarVisitor {
    type Value = f64;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(EXPECTING)
    }

    fn visit_f64<E: de::Error>(self, value: f64) -> Result<f64, E> {
        Err(old_format(value))
    }

    fn visit_i64<E: de::Error>(self, value: i64) -> Result<f64, E> {
        Err(old_format(value))
    }

    fn visit_u64<E: de::Error>(self, value: u64) -> Result<f64, E> {
        Err(old_format(value))
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<f64, A::Error> {
        let mut n: Option<String> = None;
        let mut d: Option<String> = None;
        while let Some(key) = map.next_key::<String>()? {
            let slot = match key.as_str() {
                "n" => &mut n,
                "d" => &mut d,
                other => {
                    return Err(de::Error::custom(format!(
                        "unknown field {other:?} in an exact scalar; expected only \"n\" and \"d\""
                    )))
                }
            };
            if slot.is_some() {
                return Err(de::Error::custom(format!("duplicate field {key:?}")));
            }
            *slot = Some(map.next_value::<String>()?);
        }
        let n = n.ok_or_else(|| de::Error::missing_field("n"))?;
        let d = d.ok_or_else(|| de::Error::missing_field("d"))?;
        scalar_to_f64(&n, &d).map_err(de::Error::custom)
    }
}

fn serialize_scalar<S: Serializer>(value: f64, serializer: S) -> Result<S::Ok, S::Error> {
    let (n, d) = f64_to_scalar(value).map_err(ser::Error::custom)?;
    let mut map = serializer.serialize_map(Some(2))?;
    map.serialize_entry("n", &n)?;
    map.serialize_entry("d", &d)?;
    map.end()
}

/// `#[serde(with = "crate::exact::scalar")]` for an f64 graph value.
pub mod scalar {
    use super::*;

    pub fn serialize<S: Serializer>(value: &f64, serializer: S) -> Result<S::Ok, S::Error> {
        serialize_scalar(*value, serializer)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<f64, D::Error> {
        deserializer.deserialize_any(ScalarVisitor)
    }
}

/// A bracket threshold: an exact scalar, or `null` for the unbounded top
/// bracket, held as `f64::INFINITY`.
pub mod threshold {
    use super::*;

    struct ThresholdVisitor;

    impl<'de> Visitor<'de> for ThresholdVisitor {
        type Value = f64;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            write!(
                formatter,
                "{EXPECTING}, or null for the unbounded top bracket"
            )
        }

        fn visit_unit<E: de::Error>(self) -> Result<f64, E> {
            Ok(f64::INFINITY)
        }

        fn visit_none<E: de::Error>(self) -> Result<f64, E> {
            Ok(f64::INFINITY)
        }

        fn visit_f64<E: de::Error>(self, value: f64) -> Result<f64, E> {
            Err(old_format(value))
        }

        fn visit_i64<E: de::Error>(self, value: i64) -> Result<f64, E> {
            Err(old_format(value))
        }

        fn visit_u64<E: de::Error>(self, value: u64) -> Result<f64, E> {
            Err(old_format(value))
        }

        fn visit_map<A: MapAccess<'de>>(self, map: A) -> Result<f64, A::Error> {
            ScalarVisitor.visit_map(map)
        }
    }

    pub fn serialize<S: Serializer>(value: &f64, serializer: S) -> Result<S::Ok, S::Error> {
        if *value == f64::INFINITY {
            serializer.serialize_unit()
        } else {
            serialize_scalar(*value, serializer)
        }
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<f64, D::Error> {
        deserializer.deserialize_any(ThresholdVisitor)
    }
}

/// `#[serde(with = "crate::exact::by_status")]` for status-indexed f64 values.
pub mod by_status {
    use super::*;
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize)]
    #[serde(deny_unknown_fields)]
    struct Exact {
        #[serde(with = "super::scalar")]
        single: f64,
        #[serde(with = "super::scalar")]
        married_joint: f64,
        #[serde(with = "super::scalar")]
        married_separate: f64,
        #[serde(with = "super::scalar")]
        head_of_household: f64,
        #[serde(with = "super::scalar")]
        qualifying_widow: f64,
    }

    pub fn serialize<S: Serializer>(
        value: &ByStatus<f64>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        Exact {
            single: value.single,
            married_joint: value.married_joint,
            married_separate: value.married_separate,
            head_of_household: value.head_of_household,
            qualifying_widow: value.qualifying_widow,
        }
        .serialize(serializer)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<ByStatus<f64>, D::Error> {
        let exact = Exact::deserialize(deserializer)?;
        Ok(ByStatus {
            single: exact.single,
            married_joint: exact.married_joint,
            married_separate: exact.married_separate,
            head_of_household: exact.head_of_household,
            qualifying_widow: exact.qualifying_widow,
        })
    }
}

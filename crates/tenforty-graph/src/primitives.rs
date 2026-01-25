use crate::graph::Bracket;

/// Compute progressive tax from bracket table.
/// Brackets must be sorted by threshold in ascending order.
/// Returns the total tax owed.
pub fn bracket_tax(brackets: &[Bracket], income: f64) -> f64 {
    if income <= 0.0 || brackets.is_empty() {
        return 0.0;
    }

    let mut tax = 0.0;
    let mut prev_threshold = 0.0;

    for bracket in brackets {
        if income <= bracket.threshold {
            tax += (income - prev_threshold) * bracket.rate;
            return tax;
        }
        tax += (bracket.threshold - prev_threshold) * bracket.rate;
        prev_threshold = bracket.threshold;
    }

    let last = brackets.last().unwrap();
    tax += (income - last.threshold) * last.rate;
    tax
}

/// Compute the marginal tax rate at a given income level.
/// Returns the rate of the bracket containing the income.
pub fn marginal_rate(brackets: &[Bracket], income: f64) -> f64 {
    if income <= 0.0 || brackets.is_empty() {
        return 0.0;
    }

    for bracket in brackets {
        if income <= bracket.threshold {
            return bracket.rate;
        }
    }

    brackets.last().map(|b| b.rate).unwrap_or(0.0)
}

/// Compute phase-out benefit reduction.
/// base: starting benefit amount
/// threshold: AGI threshold where phase-out begins
/// rate: reduction rate (e.g., 0.05 means 5% reduction per dollar over threshold)
/// agi: taxpayer's AGI
/// Returns the reduced benefit (floored at 0).
pub fn phase_out(base: f64, threshold: f64, rate: f64, agi: f64) -> f64 {
    if agi <= threshold {
        return base;
    }

    let reduction = (agi - threshold) * rate;
    (base - reduction).max(0.0)
}

/// Compute the gradient of phase_out with respect to AGI.
/// Returns 0 if below threshold, -rate if in phase-out range, 0 if fully phased out.
pub fn phase_out_gradient(base: f64, threshold: f64, rate: f64, agi: f64) -> f64 {
    if agi <= threshold {
        return 0.0;
    }

    let reduction = (agi - threshold) * rate;
    if reduction >= base {
        0.0
    } else {
        -rate
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn federal_2024_single_brackets() -> Vec<Bracket> {
        vec![
            Bracket { threshold: 11600.0, rate: 0.10 },
            Bracket { threshold: 47150.0, rate: 0.12 },
            Bracket { threshold: 100525.0, rate: 0.22 },
            Bracket { threshold: 191950.0, rate: 0.24 },
            Bracket { threshold: 243725.0, rate: 0.32 },
            Bracket { threshold: 609350.0, rate: 0.35 },
            Bracket { threshold: f64::INFINITY, rate: 0.37 },
        ]
    }

    #[test]
    fn test_bracket_tax_zero_income() {
        let brackets = federal_2024_single_brackets();
        assert_eq!(bracket_tax(&brackets, 0.0), 0.0);
        assert_eq!(bracket_tax(&brackets, -1000.0), 0.0);
    }

    #[test]
    fn test_bracket_tax_first_bracket() {
        let brackets = federal_2024_single_brackets();
        assert_eq!(bracket_tax(&brackets, 10000.0), 1000.0);
    }

    #[test]
    fn test_bracket_tax_second_bracket() {
        let brackets = federal_2024_single_brackets();
        let expected = 11600.0 * 0.10 + (20000.0 - 11600.0) * 0.12;
        let actual = bracket_tax(&brackets, 20000.0);
        assert!((actual - expected).abs() < 0.01);
    }

    #[test]
    fn test_bracket_tax_multiple_brackets() {
        let brackets = federal_2024_single_brackets();
        let expected = 11600.0 * 0.10
            + (47150.0 - 11600.0) * 0.12
            + (75000.0 - 47150.0) * 0.22;
        let actual = bracket_tax(&brackets, 75000.0);
        assert!((actual - expected).abs() < 0.01);
    }

    #[test]
    fn test_marginal_rate() {
        let brackets = federal_2024_single_brackets();
        assert_eq!(marginal_rate(&brackets, 5000.0), 0.10);
        assert_eq!(marginal_rate(&brackets, 30000.0), 0.12);
        assert_eq!(marginal_rate(&brackets, 75000.0), 0.22);
        assert_eq!(marginal_rate(&brackets, 150000.0), 0.24);
    }

    #[test]
    fn test_phase_out_below_threshold() {
        assert_eq!(phase_out(2000.0, 100000.0, 0.05, 50000.0), 2000.0);
    }

    #[test]
    fn test_phase_out_partial() {
        let result = phase_out(2000.0, 100000.0, 0.05, 110000.0);
        assert_eq!(result, 2000.0 - 10000.0 * 0.05);
    }

    #[test]
    fn test_phase_out_complete() {
        let result = phase_out(2000.0, 100000.0, 0.05, 150000.0);
        assert_eq!(result, 0.0);
    }

    #[test]
    fn test_phase_out_gradient() {
        assert_eq!(phase_out_gradient(2000.0, 100000.0, 0.05, 50000.0), 0.0);
        assert_eq!(phase_out_gradient(2000.0, 100000.0, 0.05, 110000.0), -0.05);
        assert_eq!(phase_out_gradient(2000.0, 100000.0, 0.05, 200000.0), 0.0);
    }
}

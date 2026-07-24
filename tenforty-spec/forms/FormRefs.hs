-- | Typed cross-form output handles.
--
-- This module imports no form module, so importers reference these handles
-- without depending on the exporting form's module — the form-module graph
-- stays acyclic even where two forms reference each other's lines. Each
-- handle names a (form, line); the importing form's year fixes resolution.
module FormRefs where

import TenForty (Dollars, LineRef, lineRef)

ca540L17 :: LineRef Dollars
ca540L17 = lineRef "ca_540" "L17"

caFtb3514L18 :: LineRef Dollars
caFtb3514L18 = lineRef "ca_ftb_3514" "L18"

caScheduleCaTotalAdd :: LineRef Dollars
caScheduleCaTotalAdd = lineRef "ca_schedule_ca" "TOTAL_ADD"

caScheduleCaTotalSub :: LineRef Dollars
caScheduleCaTotalSub = lineRef "ca_schedule_ca" "TOTAL_SUB"

us1040L11 :: LineRef Dollars
us1040L11 = lineRef "us_1040" "L11"

us1040L12final :: LineRef Dollars
us1040L12final = lineRef "us_1040" "L12Final"

us1040L15 :: LineRef Dollars
us1040L15 = lineRef "us_1040" "L15"

us1040L15PreQbi :: LineRef Dollars
us1040L15PreQbi = lineRef "us_1040" "L15_pre_qbi"

us1040L16 :: LineRef Dollars
us1040L16 = lineRef "us_1040" "L16"

us1040L18 :: LineRef Dollars
us1040L18 = lineRef "us_1040" "L18"

us1040L22 :: LineRef Dollars
us1040L22 = lineRef "us_1040" "L22"

us1040L6b :: LineRef Dollars
us1040L6b = lineRef "us_1040" "L6b"

us1040L9 :: LineRef Dollars
us1040L9 = lineRef "us_1040" "L9"

us1040Qcgws4 :: LineRef Dollars
us1040Qcgws4 = lineRef "us_1040" "qcgws_4"

us1040Qcgws5 :: LineRef Dollars
us1040Qcgws5 = lineRef "us_1040" "qcgws_5"

usForm2441L11 :: LineRef Dollars
usForm2441L11 = lineRef "us_form_2441" "L11"

usForm6251L11 :: LineRef Dollars
usForm6251L11 = lineRef "us_form_6251" "L11"

usForm8812L14 :: LineRef Dollars
usForm8812L14 = lineRef "us_form_8812" "L14"

usForm8812L27 :: LineRef Dollars
usForm8812L27 = lineRef "us_form_8812" "L27"

usForm8863L19 :: LineRef Dollars
usForm8863L19 = lineRef "us_form_8863" "L19"

usForm8863L9 :: LineRef Dollars
usForm8863L9 = lineRef "us_form_8863" "L9"

usForm8959L18 :: LineRef Dollars
usForm8959L18 = lineRef "us_form_8959" "L18"

usForm8960L17 :: LineRef Dollars
usForm8960L17 = lineRef "us_form_8960" "L17"

usForm8995L16 :: LineRef Dollars
usForm8995L16 = lineRef "us_form_8995" "L16"

usSchedule1L1 :: LineRef Dollars
usSchedule1L1 = lineRef "us_schedule_1" "L1"

usSchedule1L10 :: LineRef Dollars
usSchedule1L10 = lineRef "us_schedule_1" "L10"

usSchedule1L26 :: LineRef Dollars
usSchedule1L26 = lineRef "us_schedule_1" "L26"

usSchedule2L21 :: LineRef Dollars
usSchedule2L21 = lineRef "us_schedule_2" "L21"

usSchedule2L3 :: LineRef Dollars
usSchedule2L3 = lineRef "us_schedule_2" "L3"

usSchedule3L15 :: LineRef Dollars
usSchedule3L15 = lineRef "us_schedule_3" "L15"

usSchedule3L8 :: LineRef Dollars
usSchedule3L8 = lineRef "us_schedule_3" "L8"

usScheduleAL17 :: LineRef Dollars
usScheduleAL17 = lineRef "us_schedule_a" "L17"

usScheduleDL15 :: LineRef Dollars
usScheduleDL15 = lineRef "us_schedule_d" "L15"

usScheduleDL21 :: LineRef Dollars
usScheduleDL21 = lineRef "us_schedule_d" "L21"

usScheduleSeL10 :: LineRef Dollars
usScheduleSeL10 = lineRef "us_schedule_se" "L10"

usScheduleSeL11 :: LineRef Dollars
usScheduleSeL11 = lineRef "us_schedule_se" "L11"

usScheduleSeL4a :: LineRef Dollars
usScheduleSeL4a = lineRef "us_schedule_se" "L4a"

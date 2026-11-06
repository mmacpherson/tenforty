module USScheduleEIC_2024
  ( usScheduleEIC_2024,
  )
where

import ScheduleEICCommon
import Tables2024
import TenForty

usScheduleEIC_2024 :: Either FormError Form
usScheduleEIC_2024 =
  scheduleEIC
    EicYear
      { eicYear = 2024,
        eicColumns =
          ( EicColumn eitcPhaseInRate0QC2024 eitcPhaseOutRate0QC2024 eitcPhaseInEnds0QC2024 eitcMaxCredit0QC2024 eitcPhaseOutThreshold0QC2024 eitcCompletedPhaseOut0QC2024,
            EicColumn eitcPhaseInRate1QC2024 eitcPhaseOutRate1QC2024 eitcPhaseInEnds1QC2024 eitcMaxCredit1QC2024 eitcPhaseOutThreshold1PlusQC2024 eitcCompletedPhaseOut1QC2024,
            EicColumn eitcPhaseInRate2QC2024 eitcPhaseOutRate2PlusQC2024 eitcPhaseInEnds2PlusQC2024 eitcMaxCredit2QC2024 eitcPhaseOutThreshold1PlusQC2024 eitcCompletedPhaseOut2QC2024,
            EicColumn eitcPhaseInRate3PlusQC2024 eitcPhaseOutRate2PlusQC2024 eitcPhaseInEnds2PlusQC2024 eitcMaxCredit3PlusQC2024 eitcPhaseOutThreshold1PlusQC2024 eitcCompletedPhaseOut3PlusQC2024
          ),
        eicInvestmentIncomeLimit = eitcInvestmentIncomeLimit2024,
        eicEligibleStatus = byStatus 1 1 0 1 1
      }

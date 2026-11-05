module USScheduleEIC_2025
  ( usScheduleEIC_2025,
  )
where

import ScheduleEICCommon
import Tables2025
import TenForty

usScheduleEIC_2025 :: Either FormError Form
usScheduleEIC_2025 =
  scheduleEIC
    EicYear
      { eicYear = 2025,
        eicColumns =
          ( EicColumn eitcPhaseInRate0QC2025 eitcPhaseOutRate0QC2025 eitcPhaseInEnds0QC2025 eitcMaxCredit0QC2025 eitcPhaseOutThreshold0QC2025 eitcCompletedPhaseOut0QC2025,
            EicColumn eitcPhaseInRate1QC2025 eitcPhaseOutRate1QC2025 eitcPhaseInEnds1QC2025 eitcMaxCredit1QC2025 eitcPhaseOutThreshold1PlusQC2025 eitcCompletedPhaseOut1QC2025,
            EicColumn eitcPhaseInRate2QC2025 eitcPhaseOutRate2PlusQC2025 eitcPhaseInEnds2PlusQC2025 eitcMaxCredit2QC2025 eitcPhaseOutThreshold1PlusQC2025 eitcCompletedPhaseOut2QC2025,
            EicColumn eitcPhaseInRate3PlusQC2025 eitcPhaseOutRate2PlusQC2025 eitcPhaseInEnds2PlusQC2025 eitcMaxCredit3PlusQC2025 eitcPhaseOutThreshold1PlusQC2025 eitcCompletedPhaseOut3PlusQC2025
          ),
        eicInvestmentIncomeLimit = eitcInvestmentIncomeLimit2025,
        eicEligibleStatus = byStatus 1 1 0 1 1
      }

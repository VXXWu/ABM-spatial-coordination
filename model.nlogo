extensions [
  csv
  rnd ]

breed [farmers farmer]
breed [borders border]
breed [crops crop]
patches-own [
  owner
  choice
  currentYield
  currentSpending
  currentSubsidy
  currentProvidedValueSelf ; total value to self (net yield+nch bonus provided to own cells)
  currentProvidedValue ; total value (net yield+nch bonus provided)
  receivedNCHBonus
  providedNCHBonus
  NCHProviders
  NCHProvided
  heavySprayCanceled
  currentScore ; total net yield from the cell

  experiencePatch
  experienceFarmer
  experienceOthers

  patchNeighbors

  unclaimed
  sameOwner

  cropWho
]
farmers-own [
  growthWeight
  ownPatches
  neighborsLeft

  sizeFarm
  riskProfile
  decayRate
  currentPatchCount
  neighborList
  potentialClusters
  currCluster

  farmerScore
  farmerNetProduction

  scaleFarmer
  scaleOthers

  nchStrategy
  carePatch
  careFarm
  careLandscape

  proposeRisk
  switchDifficulty
  switchedGroup

  nchYieldMemory
;  nchYieldLand
  hsYieldMemory
;  hsYieldLand
  maxYieldMemory
;  maxRecentYieldLand
  proposedStrategy
  prevStrategy
]
links-own [trustIndex]

globals [
  endRun

  totalPatches
  avgPatches
  choiceColors
  baseYield
  nchYield
  nchBoost
  nchNeighborhood
  lightSprayBoost lightSprayCost
  heavySprayBoost heavySprayCost heavySprayBlockNeighborhood
  maxYield
  minPlotSize

  totalGrowthWeight

  totalNetProduction
  totalFarmerScore
  clusters
  clusterOptions
  clusterOptionScores
  clusterOptionFarmers

  iterations
  loops
  scaleFactors
  data
  fitness
  parameters
  outputData

  edgePatchPercent

  overallFarmerScore ; Score/total patches
  overallNetProduction ; Net production/total patches
  overallNCHCells ; NCH Cells/total patches
  overallNCHStrategy ; NCH strategy/total farmers
  overallDeviation; Average deviation from mean score
  overallHSCells; HS Cells/total patches
  overallNCHBenefit; NCH benefit provided per NCH cell
  overallNCHEdge; percent of NCH cells on edge patches/percent of patches that are edge
  overallStrategySwitches
  overallNCHSwitches
  overallPopularStrategy

  overallHS_NonNCH; HS Cells/total patches for only non-NCH strategy holders
  overallHS_NCH; HS Cells/total patches for only NCH strategy holders
  overallNCH_NonNCH; NCH Cells/total patches for only non-NCH strategy holders
  overallNCH_NCH; NCH Cells/total patches for only NCH strategy holder
  overallNCHBenefit_NonNCH; NCH benefit for non-NCH strategy holders / total NCH cells
  overallNCHBenefit_NCH; NCH benefit for NCH strategy holders / total NCH cells

  NCHEdgeTotal

  listFarmerScore
  listNetProduction
  listNCHCells
  listNCHStrategy
  listDeviation
  listHSCells
  listNCHBenefit
  listNCHEdge
  listStrategySwitches
  listNCHSwitches
  listPopularStrategy

  listHS_NonNCH; HS Cells/total patches for only non-NCH strategy holders
  listHS_NCH; HS Cells/total patches for only NCH strategy holders
  listNCH_NonNCH; NCH Cells/total patches for only non-NCH strategy holders
  listNCH_NCH; NCH Cells/total patches for only NCH strategy holder
  listNCHBenefit_NonNCH
  listNCHBenefit_NCH

  sizeStdDev
  sizeRange
  largestPlot
  coeffVariation
  giniCoeff

  monteCarlo
  validationSplits
]

to setup-monte-carlo
  clear-all
  reset-ticks

  set subsidy 0

  set endRun false

  set clusters []
  set monteCarlo True
  set validationSplits 50

  set overallFarmerScore 0
  set overallNetProduction 0
  set overallNCHCells 0
  set overallNCHStrategy 0
  set overallNCHBenefit 0
  set overallDeviation 0
  set overallHSCells 0
  set overallNCHBenefit 0
  set overallNCHEdge 0
  set overallStrategySwitches 0
  set overallNCHSwitches 0
  set overallPopularStrategy 0

  set overallHS_NonNCH 0; HS Cells/total patches for only non-NCH strategy holders
  set overallHS_NCH 0; HS Cells/total patches for only NCH strategy holders
  set overallNCH_NonNCH 0; NCH Cells/total patches for only non-NCH strategy holders
  set overallNCH_NCH 0; NCH Cells/total patches for only NCH strategy holder
  set overallNCHBenefit_NonNCH 0
  set overallNCHBenefit_NCH 0

  set NCHEdgeTotal 0

  set listFarmerScore []
  set listNetProduction []
  set listNCHCells []
  set listNCHStrategy []
  set listDeviation []
  set listHSCells []
  set listNCHBenefit []
  set listNCHEdge []
  set listStrategySwitches []
  set listNCHSwitches []
  set listPopularStrategy []

  set listHS_NonNCH []
  set listHS_NCH []
  set listNCH_NonNCH []
  set listNCH_NCH []
  set listNCHBenefit_NonNCH []
  set listNCHBenefit_NCH []

  set trustIndexMean random-float 1
  set heterogeneityIndex random-float 2
  set carePatchMean random-float 1
  set careFarmMean random-float 1
  set careLandscapeMean random-float 1
  ; 5 to 10
  set initialExperience 5 + ( random-float 5 )
  set nchPercent random-float 1
  set memory 5 + (random 25)
  set percentageSD random-float 0.1

  set careOtherStrategy True
  set understandSpatial False

  set choiceColors [95 65 45 15]
  set-default-shape borders "line"
  set-default-shape farmers "person farmer"

  set totalPatches (sideLength ^ 2)
  set avgPatches totalPatches / numFarms

  set fitness 0
  set iterations 0

  set nchBoost 2
  set overallStrategySwitches 0
  set overallNCHSwitches 0
  set totalNetProduction 0
  set totalFarmerScore 0

  set overallFarmerScore 0

  ; val 0
  set baseYield 5

  ; val 1
  set nchYield 0
  set nchNeighborhood 2

  ; val 2
  set lightSprayBoost 2
  set lightSprayCost 1

  ; val 3
  set heavySprayBoost 7
  set heavySprayCost 2
  set heavySprayBlockNeighborhood 1

  set maxYield 15
  ; make board sideLength x sideLength
  resize-world 0 (sideLength - 1) 0 (sideLength - 1)
  set-patch-size 320 / (sideLength)

  ;; seed the landscape
  ask patches [set owner -99
    set choice 0
    update-crop-image]

  populate-landscape

  make-borders

  seed-new-choices
  initial-run

  reset-ticks
end


to setup
  clear-all

  reset-ticks

  set subsidy 0

  set monteCarlo False
  set validationSplits 50

  set minPlotSize 4

  set endRun false

  if( (sideLength ^ 2) / numFarms ) < minPlotSize
  [ set endRun true
    stop ]

  set overallFarmerScore 0
  set overallNetProduction 0
  set overallNCHCells 0
  set overallNCHStrategy 0
  set overallNCHBenefit 0
  set overallDeviation 0
  set overallHSCells 0
  set overallNCHBenefit 0
  set overallNCHEdge 0
  set overallStrategySwitches 0
  set overallNCHSwitches 0
  set overallPopularStrategy 0

  set overallHS_NonNCH 0; HS Cells/total patches for only non-NCH strategy holders
  set overallHS_NCH 0; HS Cells/total patches for only NCH strategy holders
  set overallNCH_NonNCH 0; NCH Cells/total patches for only non-NCH strategy holders
  set overallNCH_NCH 0; NCH Cells/total patches for only NCH strategy holder
  set overallNCHBenefit_NonNCH 0
  set overallNCHBenefit_NCH 0

  set NCHEdgeTotal 0

  set listFarmerScore []
  set listNetProduction []
  set listNCHCells []
  set listNCHStrategy []
  set listDeviation []
  set listHSCells []
  set listNCHBenefit []
  set listNCHEdge []
  set listStrategySwitches []
  set listNCHSwitches []
  set listPopularStrategy []

  set listHS_NonNCH []
  set listHS_NCH []
  set listNCH_NonNCH []
  set listNCH_NCH []
  set listNCHBenefit_NonNCH []
  set listNCHBenefit_NCH []

  file-close-all
  file-open "Calibration Data/calibration_max_vals.csv"
  set scaleFactors csv:from-row file-read-line
  file-close

  if country = "Vietnam" [
    set data csv:from-file "Calibration Data/calibration_data_vietnam.csv"
    set parameters csv:from-file "Calibration Data/calibrated_parameters_vietnam_final.csv"
  ]
  if country = "Cambodia"
  [
    set data csv:from-file "Calibration Data/calibration_data_cambodia.csv"
    set parameters csv:from-file "Calibration Data/calibrated_parameters_cambodia_final.csv"
  ]
  if country = "None" [
    set data csv:from-file "Calibration Data/calibration_data_vietnam.csv"
  ]
  file-close

  if country != "None" [
    set parameters item 0 parameters

    set memory item 0 parameters
    set nchPercent item 1 parameters
    set trustIndexMean item 2 parameters
    set initialExperience item 3 parameters
    set carePatchMean item 4 parameters
    set careFarmMean item 5 parameters
    set careLandscapeMean item 6 parameters
    set careOtherStrategy true
    set understandSpatial false
    set percentageSD 0.1
  ]

  ; blue green yellow red (base, NCH, LS, HS)
  set choiceColors [95 65 45 15] ;; one color for each choice!
  set-default-shape borders "line"
  set-default-shape farmers "person farmer"

  set totalPatches (sideLength ^ 2)
  set avgPatches totalPatches / numFarms

  set totalGrowthWeight 0

  set fitness 0
  set clusters []
  set iterations 0

  set overallStrategySwitches 0
  set overallNCHSwitches 0
  set totalNetProduction 0
  set totalFarmerScore 0

  set overallFarmerScore 0

  set nchBoost 2
  ; val 0
  set baseYield 5

  ; val 1
  set nchYield 0
  set nchNeighborhood 2

  ; val 2
  set lightSprayBoost 2
  set lightSprayCost 1

  ; val 3
  set heavySprayBoost 7
  set heavySprayCost 2
  set heavySprayBlockNeighborhood 1

  set maxYield 15
  resize-world 0 (sideLength - 1) 0 (sideLength - 1)
  set-patch-size 320 / (sideLength)

  ;; seed the landscape
  ask patches [set owner -99
    set choice 0

    update-crop-image
  ]

  populate-landscape

  make-borders

  seed-new-choices
  initial-run

  reset-ticks
end

to reset-model

  set listFarmerScore lput overallFarmerScore listFarmerScore
  set listNetProduction lput overallNetProduction listNetProduction
  set listNCHCells lput overallNCHCells listNCHCells
  set listNCHStrategy lput overallNCHStrategy listNCHStrategy
  set listDeviation lput overallDeviation listDeviation
  set listHSCells lput overallHSCells listHSCells
  set listNCHEdge lput ( overallNCHEdge / NCHEdgeTotal) listNCHEdge
  set listNCHBenefit lput overallNCHBenefit listNCHBenefit
  set listStrategySwitches lput overallStrategySwitches listStrategySwitches
  set listNCHSwitches lput overallNCHSwitches listNCHSwitches
  set listPopularStrategy lput overallPopularStrategy listPopularStrategy

  set listHS_NonNCH lput overallHS_NonNCH listHS_NonNCH
  set listHS_NCH lput overallHS_NCH listHS_NCH
  set listNCH_NonNCH lput overallNCH_NonNCH listNCH_NonNCH
  set listNCH_NCH lput overallNCH_NCH listNCH_NCH
  set listNCHBenefit_NonNCH lput overallNCHBenefit_NonNCH listNCHBenefit_NonNCH
  set listNCHBenefit_NCH lput overallNCHBenefit_NCH listNCHBenefit_NCH

  set fitness 0
  set iterations 0

  set overallFarmerScore 0
  set overallNetProduction 0
  set overallNCHCells 0
  set overallNCHStrategy 0
  set overallNCHBenefit 0
  set overallDeviation 0
  set overallHSCells 0
  set overallNCHBenefit 0
  set overallStrategySwitches 0
  set overallNCHSwitches 0
  set overallPopularStrategy 0

  set overallHS_NonNCH 0; HS Cells/total patches for only non-NCH strategy holders
  set overallHS_NCH 0; HS Cells/total patches for only NCH strategy holders
  set overallNCH_NonNCH 0; NCH Cells/total patches for only non-NCH strategy holders
  set overallNCH_NCH 0; NCH Cells/total patches for only NCH strategy holder
  set overallNCHBenefit_NonNCH 0
  set overallNCHBenefit_NCH 0

  set NCHEdgeTotal 0

  set totalNetProduction 0
  set totalFarmerScore 0

  let tot nchPercent * numFarms
  let upper ceiling tot
  let lower floor tot
  let compare random-float 1
  let numNCHtoStart 0
  ifelse upper - tot >= compare [
    set numNCHtoStart lower]
  [ set numNCHtoStart upper]

  let demoFarmers n-of numNCHtoStart farmers
  ask farmers [
    let currentFarmer who
    ifelse member? one-of farmers with [who = currentFarmer] demoFarmers [
      set nchStrategy 1
    ]
    [
      set nchStrategy 0
    ]

    ifelse careOtherStrategy [
      ; 1, 2, 3, or 4 expected nch users next round (with 5 arrays since index 0 is the first array)
      set nchYieldMemory n-values 1 [initialExperience * totalPatches]
      set hsYieldMemory n-values 1 [initialExperience * totalPatches]
      set maxYieldMemory n-values 1 [initialExperience * totalPatches]
    ]
    [
      ; 1, 2, 3, or 4 expected nch users next round
      set nchYieldMemory n-values 1 [initialExperience * sizeFarm]
      set hsYieldMemory n-values 1 [initialExperience * sizeFarm]
      set maxYieldMemory n-values 1 [initialExperience * sizeFarm]
    ]

    ask farmers [
      foreach neighborList [ ?1 ->
        create-link-with farmer ?1 [
          set trustIndex random-normal trustIndexMean percentageSD
          set trustIndex max( list trustIndex 0 )
        ]
      ]
    ]
  ]

  ; neighborOwners
  ask patches [
    ; 4 types of cells
    set experiencePatch n-values 4 [n-values 1 [initialExperience]]
    set experienceFarmer n-values 4 [n-values 1 [initialExperience]]
    set experienceOthers n-values 4 [n-values 1 [initialExperience]]
  ]

  seed-new-choices
  initial-run
end

to initial-run
  ask patches [
    update-crop-image
;    set plabel currentScore
  ]
  calculate-score

;  let currData item subsidy data
;  ask links [
;    set trustIndex min (list trustIndex 1)
;  ]


  ; strategy memory
  ask farmers [
    let currentFarmer who
    set currentPatchCount 0
    ;    set numChoice1 0

    let numNonNCH count farmers with [nchStrategy = 0]
    let numNCH count farmers with [nchStrategy = 1]

    let tempScore 0
    ifelse careOtherStrategy
    [ set tempScore totalFarmerScore ]
    [ set tempScore farmerScore ]

    if nchStrategy = 1 [
      set nchYieldMemory lput tempScore nchYieldMemory
      set nchYieldMemory trim-list nchYieldMemory memory
    ]

    if nchStrategy = 0 [
      set hsYieldMemory lput tempScore hsYieldMemory
      set hsYieldMemory trim-list hsYieldMemory memory
    ]
    set maxYieldMemory lput tempScore maxYieldMemory
    set maxYieldMemory trim-list maxYieldMemory memory
  ]

  ask patches [
    ; HOW WELL THIS PATCH DOES
    ifelse understandSpatial [
      set experiencePatch update-sublist choice experiencePatch currentProvidedValueSelf
    ]
    [ set experiencePatch update-sublist choice experiencePatch currentScore ]

    set experiencePatch replace-item choice experiencePatch trim-list (item choice experiencePatch) memory

    ; How well farmer does when this cell is x type
    let currentFarmerScore [farmerScore] of farmer owner
    let currScaleFarmer [scaleFarmer] of farmer owner
    let currScaleOthers [scaleOthers] of farmer owner
    set experienceFarmer update-sublist choice experienceFarmer (currentFarmerScore / currScaleFarmer)
    set experienceFarmer replace-item choice experienceFarmer trim-list (item choice experienceFarmer) memory

    ; How well others around me are doing, either everyone or just who I know I’m aligned with strategy
    let careLandscapeBool 0
    let othersScore totalFarmerScore - currentFarmerScore

    ifelse careOtherStrategy [
      set careLandscapeBool 1
    ]
    [
      let currentNchStrategy [nchStrategy] of farmer owner
      ; if don't care about other strategy, only nch farmers care about each other
      if currentNchStrategy = 1 [
        set careLandscapeBool 1

        let oppositeStrategy farmers with [
          nchStrategy = 0
        ]
        ask oppositeStrategy [
          set othersScore othersScore - farmerScore
        ]
      ]
    ]
    if careLandscapeBool = 1 [
      set experienceOthers update-sublist choice experienceOthers (othersScore / currScaleOthers)

      set experienceOthers replace-item choice experienceOthers trim-list (item choice experienceOthers) memory
    ]
  ]

end

to go
  ; running across same landscape for each subsidy (to allow heatmap generation)
  if ticks > 0 and (ticks mod validationSplits) = 0 [
    set subsidy subsidy + 1
    reset-model
  ]

  if subsidy > 10 [
    set endRun true
    stop
  ]
  set iterations iterations + 1


  ask farmers [
    set proposedStrategy 0
    set prevStrategy nchStrategy
    set switchedGroup 0
  ]

  farmers-switch

  ; decision
  ask patches [
    let currentChoice choice
    let currentX pxcor
    let currentY pycor
    let currExperiencePatch experiencePatch
    let currExperienceFarmer experienceFarmer
    let currExperienceOthers experienceOthers

    ask farmer owner [
      let currentEstimates []

      foreach currExperiencePatch [ ?1 ->
        let currentCertEq (mean ?1) * carePatch

        set currentEstimates (lput currentCertEq currentEstimates)
      ]

      let ind 0
      foreach currExperienceFarmer [ ?1 ->
        let currentCertEq (mean ?1) * careFarm

        let oldValue item ind currentEstimates
        let newValue oldValue + (currentCertEq)
        set currentEstimates replace-item ind currentEstimates newValue
        set ind ind + 1
      ]

      set ind 0
      foreach currExperienceOthers [ ?1 ->
        let currentCertEq (mean ?1) * careLandscape

        let oldValue item ind currentEstimates
        let newValue oldValue + (currentCertEq)

        set currentEstimates replace-item ind currentEstimates newValue
        set ind ind + 1
      ]
      if nchStrategy = 1 [
        set currentEstimates (replace-item 3 currentEstimates -9999)
      ]

      let sortedEstimates reverse sort currentEstimates

      let maxEstimate item 0 sortedEstimates
      let indexMaxEstimate position maxEstimate currentEstimates


      if indexMaxEstimate = 3 and nchStrategy = 1 [
        set maxEstimate item 1 sortedEstimates
        set indexMaxEstimate position maxEstimate currentEstimates
      ]

      ask patch currentX currentY [set choice indexMaxEstimate]
    ]
  ]

  ask patches [
    update-crop-image
  ]

  calculate-score

  ; strategy memory
  ask farmers [
    let currentFarmer who
    set currentPatchCount 0

    let numNonNCH count farmers with [nchStrategy = 0]
    let numNCH count farmers with [nchStrategy = 1]

    let tempScore 0
    ifelse careOtherStrategy
    [ set tempScore totalFarmerScore ]
    [ set tempScore farmerScore ]

    if nchStrategy = 1 [
      set nchYieldMemory lput tempScore nchYieldMemory
      set nchYieldMemory trim-list nchYieldMemory memory
    ]

    if nchStrategy = 0 [
      set hsYieldMemory lput tempScore hsYieldMemory
      set hsYieldMemory trim-list hsYieldMemory memory
    ]
    set maxYieldMemory lput tempScore maxYieldMemory
    set maxYieldMemory trim-list maxYieldMemory memory
  ]

  ; patch memory
  ask patches [
    ; HOW WELL THIS PATCH DOES
    ; instead of overall provided value,
    ; just score better encapsulates? solely valuing the patch's performace
    ifelse understandSpatial [
      set experiencePatch update-sublist choice experiencePatch currentProvidedValueSelf
    ]
    [ set experiencePatch update-sublist choice experiencePatch currentScore ]

    set experiencePatch replace-item choice experiencePatch trim-list (item choice experiencePatch) memory

    ; How well farmer does when this cell is x type
    let currentFarmerScore [farmerScore] of farmer owner
    let currScaleFarmer [scaleFarmer] of farmer owner
    let currScaleOthers [scaleOthers] of farmer owner
    set experienceFarmer update-sublist choice experienceFarmer (currentFarmerScore / currScaleFarmer)
    set experienceFarmer replace-item choice experienceFarmer trim-list (item choice experienceFarmer) memory

    ; How well others around me are doing, either everyone or just who I know I’m aligned with strategy
    let careLandscapeBool 0
    let othersScore totalFarmerScore - currentFarmerScore

    ifelse careOtherStrategy [
      set careLandscapeBool 1
    ]
    [
      let currentNchStrategy [nchStrategy] of farmer owner
      ; if don't care about other strategy, only nch farmers care about each other
      if currentNchStrategy = 1 [
        set careLandscapeBool 1

        let oppositeStrategy farmers with [
          nchStrategy = 0
        ]
        ask oppositeStrategy [
          set othersScore othersScore - farmerScore
        ]
      ]
    ]
    if careLandscapeBool = 1 [
      set experienceOthers update-sublist choice experienceOthers (othersScore / currScaleOthers)

      set experienceOthers replace-item choice experienceOthers trim-list (item choice experienceOthers) memory
    ]
  ]


  ; final data run metrics
  set overallFarmerScore overallFarmerScore + (totalFarmerScore / totalPatches) / validationSplits; Score/total patches
  set overallNetProduction overallNetProduction + (totalNetProduction / totalPatches) / validationSplits; Net production/total patches

  let totalNCHCells count patches with [choice = 1]
  set overallNCHCells overallNCHCells + (totalNCHCells / totalPatches) / validationSplits

  let temp 0
  ask farmers [
    if nchStrategy = 1 [set temp temp + 1]
  ]
  set temp temp / numFarms

  set overallNCHStrategy overallNCHStrategy + temp / validationSplits

  set temp max (list temp (1 - temp) )
  set overallPopularStrategy overallPopularStrategy + temp / validationSplits

  set temp 0
  ask patches [set temp temp + receivedNCHBonus]
  let nchPatches count patches with [choice = 1]
  if nchPatches > 0 [
    set temp temp / (count patches with [choice = 1] )
  ]
  set overallNCHBenefit overallNCHBenefit + (temp) / validationSplits

  set temp []
  let temp2 0
  ask farmers [
    set temp lput (farmerScore / sizeFarm) temp ; average per plot
    set temp2 temp2 + (farmerScore / sizeFarm)
  ]
  let deviation 0
  foreach temp [ ?1 ->
    set deviation deviation + abs ( ?1 - ( (temp2) / (numFarms) ) )
  ]
  set overallDeviation overallDeviation + (deviation / numFarms) / validationSplits; Average deviation per farmer from mean score


  set temp 0
  ask patches [
    if choice = 3 [set temp temp + 1 ]
  ]
  set overallHSCells overallHSCells + (temp / totalPatches) / validationSplits

  ask farmers [
    if prevStrategy != nchStrategy [
      set overallStrategySwitches overallStrategySwitches + 1 / numFarms / validationSplits
      if nchStrategy = 1 [
        set overallNCHSwitches overallNCHSwitches + 1 / numFarms / validationSplits
      ]
    ]
  ]

  let tempNCHBenefit 0
  let tempNCHCells 0
  ask farmers with [nchStrategy = 1] [
    let currentFarmer who
    ask patches with [owner = currentFarmer] [
      if choice = 1 [
        set overallNCH_NCH overallNCH_NCH + 1 / totalPatches / validationSplits
        set tempNCHCells tempNCHCells + 1
      ]
      set tempNCHBenefit tempNCHBenefit + providedNCHBonus
    ]
  ]
  if tempNCHCells > 0 [ set overallNCHBenefit_NCH overallNCHBenefit_NCH + ( tempNCHBenefit / tempNCHCells ) / validationSplits ]

  set tempNCHBenefit 0
  set tempNCHCells 0
  ask farmers with [nchStrategy = 0] [
    let currentFarmer who
    ask patches with [owner = currentFarmer] [
      if choice = 1 [
        set overallNCH_NonNCH overallNCH_NonNCH + 1 / totalPatches / validationSplits
        set tempNCHCells tempNCHCells + 1
      ]
      set tempNCHBenefit tempNCHBenefit + providedNCHBonus
    ]
  ]
  if tempNCHCells > 0 [ set overallNCHBenefit_NonNCH overallNCHBenefit_NonNCH + ( tempNCHBenefit / tempNCHCells ) / validationSplits ]

  if totalNCHCells > 0 and edgePatchPercent > 0[
    let tempNCHEdge count patches with [choice = 1 and any? neighbors with [owner != [owner] of myself]]
    set overallNCHEdge overallNCHEdge + ( (tempNCHEdge / totalNCHCells ) / edgePatchPercent )
    set NCHEdgeTotal NCHEdgeTotal + 1
  ]

  tick
end

to populate-landscape

  if displayImages [ ask patches [sprout-crops 1 [set shape "crop-base"]] ]

  ifelse uniformPlot [
    set numFarms 4
    create-farmers numFarms [
      set neighborList []
      set potentialClusters []
      let cornerX ifelse-value (who mod 2 = 0) [1] [4]
      let cornerY ifelse-value (who < 2) [1] [4]
      setxy cornerX cornerY
      while [any? other farmers-here] [setxy random-pxcor random-pycor]
      let currentFarmer who
      ask patch-here [set owner currentFarmer]
    ]
    ask patches with [pxcor <= 2 and pycor <= 2] [set owner 0]
    ask patches with [owner < 0 and pycor <= 2] [set owner 1]
    ask patches with [owner < 0 and pxcor <= 2] [set owner 2]
    ask patches with [owner < 0] [set owner 3]
  ]
  [
    let minWeight 1000 * heterogeneityIndex
    let prevGrowthWeight 0.001
    create-farmers numFarms [
      set shape "person farmer"
      set neighborList []
      set potentialClusters []
      set neighborsLeft true
      set sizeFarm 0
      let currentFarmer who

      set growthWeight prevGrowthWeight * ( (random-float heterogeneityIndex) + 1 )
      set prevGrowthWeight growthWeight
    ]

    let sorted-farmers sort-by [ [a b] -> [growthWeight] of b < [growthWeight] of a ] farmers
    let growthList []
    let sizeLimitList []

    foreach sorted-farmers [?1 ->
      set growthList lput ([growthWeight] of ?1) growthList
    ]

    let addBack 0
    foreach growthList [?1 ->
      let temp floor(?1 / sum growthList * totalPatches)
      if temp < minPlotSize [
        set addBack addBack + ( minPlotSize - temp )
        set temp minPlotSize
      ]
      set sizeLimitList lput temp sizeLimitList
    ]
;    will automatically chip out of largest farm
    let tot (sum sizeLimitList) - item 0 sizeLimitList
    set sizeLimitList replace-item 0 sizeLimitList (totalPatches - tot)

    let ind 0
    foreach sorted-farmers [?1 ->
      let currLimit item ind sizeLimitList

      ask ?1 [
        let currentFarmer who
        let currX 0
        let currY 0
        ifelse any? patches with [owner < 0] [
          ask one-of patches with [owner < 0] [
            set owner currentFarmer
            set currX pxcor
            set currY pycor
          ]
        ]
        [
          let edgePatches patches with [
            count neighbors4 with [ owner != [owner] of myself ] = 3
          ]

          ifelse any? edgePatches [
            ask one-of edgePatches [
              let currOwner owner

              set owner currentFarmer
              set currX pxcor
              set currY pycor
              ask neighbors4 with [owner = currOwner] [ set owner currentFarmer]
              ask farmer owner [set neighborsLeft false]
            ]
          ]
          [
            set endRun true
            stop
          ]
        ]

        setxy currX currY
        if displayImages [
          set color [255 255 255 0]
          ask links [set color [255 255 255 0]]
        ]

        set sizeFarm sizeFarm + 1

        set ownPatches patches with [owner = currentFarmer]

        while [sizeFarm < currLimit and neighborsLeft = true] [
          let surroundedPatches patches with [ owner < 0 and all? neighbors4 [ owner = currentFarmer] ]
          let addon patches with [ owner < 0 and count (neighbors4 with [owner = currentFarmer]) = 3]
          set surroundedPatches (patch-set surroundedPatches addon)

          ifelse any? surroundedPatches [
            ask surroundedPatches [
              set owner currentFarmer

              let newPatch self
              ask farmer owner [
                set ownPatches (patch-set ownPatches newPatch)
              ]
            ]
            set sizeFarm sizeFarm + ( count surroundedPatches )
          ]
          [
            let borderPatches patch-set []
            ask ownPatches [
              set borderPatches (patch-set borderPatches neighbors4 with [owner < 0])
            ]

            ifelse any? borderPatches [
              set sizeFarm sizeFarm + 1
              ask one-of borderPatches [
                set owner currentFarmer
                let newPatch self
                ask farmer owner [
                  set ownPatches (patch-set ownPatches newPatch)
                ]
              ]
            ]
            [
              set neighborsLeft false
            ]
          ]
        ]

      ]
      set ind ind + 1
    ]

    while [(count patches with [owner < 0] ) > 0 ] [
      let farmersList farmers with [neighborsLeft = true]
      let minFarmer min-one-of farmersList [sizeFarm]

      ifelse minFarmer = nobody [
        ask patches with [owner < 0] [
          set owner [owner] of one-of neighbors4
        ]
      ]
      [
        ask minFarmer [
          let currentFarmer who
          let borderPatches patch-set []
          ask ownPatches [
            set borderPatches (patch-set borderPatches neighbors4 with [owner < 0])
          ]

          ifelse any? borderPatches [
            set sizeFarm sizeFarm + 1
            ask one-of borderPatches [
              set owner currentFarmer

              let newPatch self
              ask farmer owner [
                set ownPatches (patch-set ownPatches newPatch)
              ]
            ]
          ]
          [
            set neighborsLeft false
          ]
        ]
      ]
    ]

    ; CALCULATE LANDSCAPE HETEROGENEITY METRICS
    let sizesList []
    let minSize totalPatches
    let maxSize 0

    ask farmers [
      let currentFarmer who
      set sizesList lput sizeFarm sizesList
      set minSize min (list minSize sizeFarm)
      set maxSize max (list maxSize sizeFarm)
    ]
    set sizeStdDev standard-deviation sizesList
    set sizeRange (maxSize - minSize) / totalPatches

    set largestPlot (max [sizeFarm] of farmers) / totalPatches
    set coeffVariation (sizeStdDev / (totalPatches / numFarms) )

    set giniCoeff 0
    ask farmers [
      let currentFarmer who
      let currSize sizeFarm
      ask farmers [
        if who != currentFarmer [
          set giniCoeff giniCoeff + abs(currSize - sizeFarm)
        ]
      ]
    ]
    ; half of mean rel diff
    set giniCoeff giniCoeff / (2 * numFarms * totalPatches)
  ]

  ask patches [
    let currentOwner owner
    ask neighbors[
      let neighborOwner owner

      if neighborOwner != currentOwner [
        ask farmer currentOwner [
          if (position neighborOwner neighborList = false) [
            set neighborList lput neighborOwner neighborList
          ]
        ]
      ]
    ]

    let currX pxcor
    let currY pycor

    ; top left corner
    let currNeighbors patches with [
      (pxcor = currX - 1 and pycor = currY + 1) or
      (pxcor = currX - 1 and pycor = currY) or
      (pxcor = currX and pycor = currY + 1)
    ]
    let diffOwners []
    let edge false
    ask currNeighbors [
      if owner != currentOwner [
        set diffOwners lput owner diffOwners
        set edge true
      ]
    ]
    set diffOwners remove-duplicates diffOwners
    set diffOwners lput currentOwner diffOwners
    set diffOwners sort diffOwners
    if length diffOwners > 1 [
      let clusterIndex position diffOwners clusters
      ifelse clusterIndex = false [
        set clusters lput diffOwners clusters
        ask farmer currentOwner [ set potentialClusters lput ( (length clusters) - 1 ) potentialClusters]
      ]
      [
        ask farmer currentOwner [ set potentialClusters lput clusterIndex potentialClusters]
      ]
    ]

    ; top right corner
    set currNeighbors patches with [
      (pxcor = currX + 1 and pycor = currY + 1) or
      (pxcor = currX + 1 and pycor = currY) or
      (pxcor = currX and pycor = currY + 1)
    ]
    set diffOwners []
    set edge false
    ask currNeighbors [
      if owner != currentOwner [
        set diffOwners lput owner diffOwners
        set edge true
      ]
    ]
    set diffOwners remove-duplicates diffOwners
    set diffOwners lput currentOwner diffOwners
    set diffOwners sort diffOwners
    if length diffOwners > 1 [
      let clusterIndex position diffOwners clusters
      ifelse clusterIndex = false [
        set clusters lput diffOwners clusters
        ask farmer currentOwner [ set potentialClusters lput ( (length clusters) - 1 ) potentialClusters]
      ]
      [
        ask farmer currentOwner [ set potentialClusters lput clusterIndex potentialClusters]
      ]
    ]

    ; bottom left corner
    set currNeighbors patches with [
      (pxcor = currX - 1 and pycor = currY - 1) or
      (pxcor = currX - 1 and pycor = currY) or
      (pxcor = currX and pycor = currY - 1)
    ]
    set diffOwners []
    set edge false
    ask currNeighbors [
      if owner != currentOwner [
        set diffOwners lput owner diffOwners
        set edge true
      ]
    ]
    set diffOwners remove-duplicates diffOwners
    set diffOwners lput currentOwner diffOwners
    set diffOwners sort diffOwners
    if length diffOwners > 1 [
      let clusterIndex position diffOwners clusters
      ifelse clusterIndex = false [
        set clusters lput diffOwners clusters
        ask farmer currentOwner [ set potentialClusters lput ( (length clusters) - 1 ) potentialClusters]
      ]
      [
        ask farmer currentOwner [ set potentialClusters lput clusterIndex potentialClusters]
      ]
    ]

    ; bottom right corner
    set currNeighbors patches with [
      (pxcor = currX + 1 and pycor = currY - 1) or
      (pxcor = currX + 1 and pycor = currY) or
      (pxcor = currX and pycor = currY - 1)
    ]
    set diffOwners []
    set edge false
    ask currNeighbors [
      if owner != currentOwner [
        set diffOwners lput owner diffOwners
        set edge true
      ]
    ]
    set diffOwners remove-duplicates diffOwners
    set diffOwners lput currentOwner diffOwners
    set diffOwners sort diffOwners
    if length diffOwners > 1 [
      let clusterIndex position diffOwners clusters
      ifelse clusterIndex = false [
        set clusters lput diffOwners clusters
        ask farmer currentOwner [ set potentialClusters lput ( (length clusters) - 1 ) potentialClusters]
      ]
      [
        ask farmer currentOwner [ set potentialClusters lput clusterIndex potentialClusters]
      ]
    ]
  ]

  ask farmers [
    set potentialClusters remove-duplicates potentialClusters
    set potentialClusters sort potentialClusters
  ]

  let tot nchPercent * numFarms
  let upper ceiling tot
  let lower floor tot
  let compare random-float 1
  let numNCHtoStart 0
  ifelse upper - tot >= compare [
    set numNCHtoStart lower]
  [ set numNCHtoStart upper]

  ; random "demonstration" plots of nch strategy
  let demoFarmers n-of numNCHtoStart farmers
  ; count farmsize for farmers and set risk preferences
  ask farmers [
    let currentFarmer who

    set scaleFarmer sizeFarm
    set scaleOthers (totalPatches - sizeFarm)

    ; nchStrategy
    ifelse member? one-of farmers with [who = currentFarmer] demoFarmers [
      set nchStrategy 1
    ]
    [
      set nchStrategy 0
    ]

    ; trustIndex SETTING
    ask farmers [
      foreach neighborList [ ?1 ->
        create-link-with farmer ?1 [
          set trustIndex random-normal trustIndexMean percentageSD
          set trustIndex max( list trustIndex 0 )
          set thickness 0.05
        ]
      ]
    ]

    set carePatch random-normal carePatchMean percentageSD
    set carePatch max(list carePatch 0)
    set careFarm random-normal careFarmMean percentageSD
    set careFarm max(list careFarm 0)
    set careLandscape random-normal careLandscapeMean percentageSD
    set careLandscape max(list careLandscape 0)

;      ; 1, 2, 3, or 4 expected nch users next round (with 5 arrays since index 0 is the first array)
    ; their memory of performance depends on if they care about the landscape or themselves
    ; can change this classification later
    ifelse careOtherStrategy [
      set nchYieldMemory n-values 1 [initialExperience * totalPatches]
      set hsYieldMemory n-values 1 [initialExperience * totalPatches]
      set maxYieldMemory n-values 1 [initialExperience * totalPatches]
    ]
    [
      set nchYieldMemory n-values 1 [initialExperience * scaleFarmer]
      set hsYieldMemory n-values 1 [initialExperience * scaleFarmer]
      set maxYieldMemory n-values 1 [initialExperience * scaleFarmer]
    ]
  ]

  ask patches [
    set experiencePatch n-values 4 [n-values 1 [initialExperience]]
    set experienceFarmer n-values 4 [n-values 1 [initialExperience]]
    set experienceOthers n-values 4 [n-values 1 [initialExperience]]
  ]

  ask links [set color [255 255 255 0]]
end

; farmer asks others to go along with nch
to farmers-switch
  set clusterOptions n-values (length clusters) [ [] ]
  set clusterOptionScores n-values (length clusters) [ [] ]
  set clusterOptionFarmers n-values (length clusters) [ [] ]

  ;farmers come together and come away with a strategy decision
  let currStrategies n-values numFarms [0]
  let currStrategyScores n-values numFarms [0]

  let numNonNCH count farmers with [nchStrategy = 0]
  let numNCH count farmers with [nchStrategy = 1]

  ask farmers [
    let currentFarmer who
    ; average trust level with each cluster
    let clustersTrustList []

    ; rank clusters by total trust with farmers in them
    foreach potentialClusters [ ?1 ->
      let currRank 0
      let otherFarmers item ?1 clusters

      foreach otherFarmers [ tempFarmer ->
        if tempFarmer != currentFarmer [
          ask farmer currentFarmer [ set currRank currRank + ([trustIndex] of link-with farmer tempFarmer) ]
        ]
      ]

      ;prioritizing clusters with at least 1 neighbor already in them
      if length (item ?1 clusterOptionFarmers) > 1 [
        set currRank currRank + numFarms
      ]

      set clustersTrustList lput currRank clustersTrustList
    ]
    let sortedTrustList reverse sort clustersTrustList

    let participateCluster false
    let wantedCluster -1
    foreach sortedTrustList [ ?1 ->
      if participateCluster = false [
        let currIndex position ?1 clustersTrustList
        let numFarmers length (item currIndex clusters)
        let avgTrust ?1 / numFarmers

        set participateCluster (random-float 1) < avgTrust
        if participateCluster [
          ; using the it'th cluster of sorted
          let pos position ?1 clustersTrustList
          set wantedCluster item pos potentialClusters
        ]
      ]
    ]
    set currCluster wantedCluster

    ; their actual proposition
    let nchPropose mean nchYieldMemory
    let hsPropose mean hsYieldMemory

    ifelse wantedCluster >= 0 [
      ifelse nchPropose >= hsPropose [
        set clusterOptions update-sublist wantedCluster clusterOptions 1
        set clusterOptionScores update-sublist wantedCluster clusterOptionScores nchPropose
        set clusterOptionFarmers update-sublist wantedCluster clusterOptionFarmers currentFarmer
      ]
      [
        set clusterOptions update-sublist wantedCluster clusterOptions 0
        set clusterOptionScores update-sublist wantedCluster clusterOptionScores hsPropose
        set clusterOptionFarmers update-sublist wantedCluster clusterOptionFarmers currentFarmer
      ]
    ]
    [
      ifelse nchPropose >= hsPropose [
        set nchStrategy 1
      ]
      [ set nchStrategy 0]
    ]
  ]

  let ind 0
  while [ind < length clusterOptions] [
    let options item ind clusterOptions
    if length options > 0 [
      let optionScores item ind clusterOptionScores
      let optionFarmers item ind clusterOptionFarmers

      let sortedScores reverse sort optionScores
      let maxEstimate item 0 sortedScores
      let selectedInd position maxEstimate optionScores
      let selectedStrategy item selectedInd options
      let selectedFarmer item selectedInd optionFarmers

      ask farmers with [who = selectedFarmer] [
        if selectedStrategy != nchStrategy[
          set nchStrategy selectedStrategy
          set switchedGroup 1
        ]
      ]
      let tempCluster [currCluster] of farmer selectedFarmer
      ask farmers with [currCluster = tempCluster and nchStrategy != selectedStrategy and who != selectedFarmer] [
        let trust [trustIndex] of link-with farmer selectedFarmer
        let adopt trust >= (random-float 1)
        if adopt [
          set nchStrategy selectedStrategy
          set switchedGroup 1
        ]
      ]
    ]
    set ind ind + 1
  ]

  set edgePatchPercent (count patches with [any? neighbors with [owner != [owner] of myself]]) / totalPatches

end


to calculate-score

  set totalNetProduction 0
  set totalFarmerScore 0

  ;; 0: base, no action
  ;; 1: NCH
  ;; 2: light spray
  ;; 3: heavy spray

  ask farmers [set farmerScore 0 set farmerNetProduction 0]

  ask patches [

    set currentYield 0
    set currentSpending 0
    set currentScore 0
    set currentSubsidy 0
    set currentProvidedValueSelf 0
    set currentProvidedValue 0
    set receivedNCHBonus 0
    set providedNCHBonus 0
    set NCHProviders []
    set NCHProvided []
    set heavySprayCanceled 0

    if choice = 0 [set currentYield baseYield]
    if choice = 1 [set currentYield nchYield]
    if choice = 2 [set currentYield baseYield + lightSprayBoost set currentSpending lightSprayCost]
    if choice = 3 [set currentYield baseYield + heavySprayBoost set currentSpending heavySprayCost]
   ]

  ask patches with [choice = 1] [
    let myX pxcor
    let myY pycor
    let currentOwner owner

    let nchBeneficiaries patches with [
      choice != 1 and choice != 3 and
      (pxcor <= (myX + nchNeighborhood)) and
      (pxcor >= (myX - nchNeighborHood)) and
      (pycor <= (myY + nchNeighborhood)) and
      (pycor >= (myY - nchNeighborHood))
    ]
    let numBenefited count nchBeneficiaries
    let tempNCHProvided NCHProvided
    ask nchBeneficiaries [
      set receivedNCHBonus receivedNCHBonus + nchBoost
      set NCHProviders lput list (myX) (myY) NCHProviders
      set tempNCHProvided lput list (pxcor) (pycor) tempNCHProvided
    ]

    set NCHProvided tempNCHProvided
    set currentSubsidy subsidy
    set providedNCHBonus numBenefited * nchBoost
  ]

  ask patches with [choice = 3] [
    let myX pxcor
    let myY pycor
    let currentOwner owner

    let heavySpraySuckers patches with [
      choice != 1 and choice != 3 and receivedNCHBonus > 0 and
      (pxcor <= (myX + heavySprayBlockNeighborhood)) and
      (pxcor >= (myX - heavySprayBlockNeighborhood)) and
      (pycor <= (myY + heavySprayBlockNeighborhood)) and
      (pycor >= (myY - heavySprayBlockNeighborhood))
    ]

    ask heavySpraySuckers [
      set heavySprayCanceled 1
      foreach NCHProviders[ coords ->
        ask patch item 0 coords item 1 coords [
          set providedNCHBonus providedNCHBonus - nchBoost
        ]
      ]
      set receivedNCHBonus 0
    ]

  ]

  ask patches [
  ; cap cell yield at 15 as in NonCropShare
    set currentScore currentYield + receivedNCHBonus
    if currentScore > 15 [
      set receivedNCHBonus receivedNCHBonus - (currentScore - 15)
      set currentScore 15
    ]
    set currentScore currentScore + currentSubsidy - currentSpending
    set currentProvidedValueSelf currentProvidedValueSelf + currentScore

    let scoreTemp currentScore
    let netProductionTemp currentScore - currentSubsidy

    ask farmer owner [
      set farmerScore farmerScore + scoreTemp
      set farmerNetProduction farmerNetProduction + netProductionTemp
    ]
  ]
  ask farmers [
    set totalFarmerScore totalFarmerScore + farmerScore
    set totalNetProduction totalNetProduction + farmerNetProduction
  ]

end

to make-borders
  ; make border around farm parcels

  ask patches [
    let currentOwner owner
    let x1 pxcor
    let y1 pycor
    ask neighbors4 [
      let x2 pxcor
      let y2 pycor
      let neighborOwner owner

     ifelse uniformPlot [
     ; makes the 3x3 plots clearer
        ifelse neighborOwner != currentOwner [
          sprout-borders 1 [set color black
            setxy mean (list x1 x2) mean (list y1 y2)
            ifelse y1 != y2 [set heading 90] [set heading 0]
            stamp die]
        ]
        [
          sprout-borders 1 [set color black
            setxy mean (list x1 x2) mean (list y1 y2)
            ifelse y1 != y2 [set heading 90] [set heading 0]
            stamp die]
        ]
      ]
      [
        if neighborOwner != currentOwner [
          sprout-borders 1 [set color black
            setxy mean (list x1 x2) mean (list y1 y2)
            ifelse y1 != y2 [set heading 90] [set heading 0]
            stamp die]

        ]
      ]
    ]
  ]

end

to-report surrounded-patches [currentFarmer]
  ask patches [
    show currentScore
  ]
  report patches with [
    (count unclaimed = 1 and count sameOwner = 3) or (count sameOwner = 4)
  ]
end

to-report replace-subitem [index1 index2 lists value]
  let old-sublist item index1 lists
  report replace-item index1 lists (replace-item index2 old-sublist value)
end

to-report update-sublist [index lists value]
  let chosenList item index lists
  set chosenList lput value chosenList
  set lists replace-item index lists chosenList
  report lists
end

to-report trim-list [oldList maxLength]
  let startPos (length oldList - (min (list maxLength (length oldList) ) ) )
  let endPos length oldList
  let newList sublist oldList startPos endPos
  report newList
end

to-report certEq [currentExperience tau careValue]
  let numEntries length currentExperience
  ifelse numEntries > 0 [
    let temp1 []
    foreach currentExperience [ ?1 ->
      let temp ?1 * careValue / numEntries
      set temp1 (lput temp temp1)
    ]
    report (sum temp1)
  ]
  [ report -9999 ]
end

to seed-new-choices
  ask patches [
    ifelse [nchStrategy] of farmer owner = 1 [
      set choice random 3
    ]
    [
      set choice random 4
    ]
  ]
end

to update-crop-image
  ifelse displayImages
  [
    set pcolor 35
    ask crops-here [
      ifelse choice = 0 [
        set shape "crop-base"
        ] [ ifelse choice = 1 [

          set shape "non-crop"

          ] [ ifelse choice = 2 [
            set shape "crop-spray-light"

          ] [

            set shape "crop-spray-heavy"
          ]

        ]
      ]
    ]
    ask farmers [ set shape "person farmer" ]
  ]
  [set pcolor item choice choiceColors]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
538
339
-1
-1
21.333333333333332
1
12
1
1
1
0
0
0
1
0
14
0
14
1
1
1
ticks
30.0

BUTTON
14
15
77
48
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
82
15
145
48
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
13
168
185
201
numFarms
numFarms
1
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
13
445
185
478
memory
memory
1
100
30.0
1
1
NIL
HORIZONTAL

PLOT
550
12
750
162
Total Yield
Time
totalNetProduction
0.0
100.0
0.0
50.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot totalNetProduction"

PLOT
551
169
751
319
Total Income
Time
totalScore
0.0
100.0
0.0
50.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot totalFarmerScore"

SLIDER
13
262
185
295
subsidy
subsidy
0
10
0.0
1
1
NIL
HORIZONTAL

SLIDER
13
307
185
340
nchPercent
nchPercent
0
1
0.22
0.01
1
NIL
HORIZONTAL

SLIDER
13
351
185
384
trustIndexMean
trustIndexMean
0
1
0.56
0.01
1
NIL
HORIZONTAL

SLIDER
13
397
185
430
initialExperience
initialExperience
0
30
8.8
1
1
NIL
HORIZONTAL

SWITCH
757
22
886
55
uniformPlot
uniformPlot
1
1
-1000

SLIDER
210
350
382
383
carePatchMean
carePatchMean
0
1
0.86
0.01
1
NIL
HORIZONTAL

SLIDER
210
396
382
429
careFarmMean
careFarmMean
0
1
0.07
0.01
1
NIL
HORIZONTAL

SLIDER
13
492
185
525
percentageSD
percentageSD
0
0.2
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
211
443
383
476
careLandscapeMean
careLandscapeMean
0
1
0.27
0.01
1
NIL
HORIZONTAL

SWITCH
757
158
927
191
careOtherStrategy
careOtherStrategy
0
1
-1000

SWITCH
757
201
914
234
understandSpatial
understandSpatial
1
1
-1000

SLIDER
756
67
928
100
sideLength
sideLength
3
30
15.0
1
1
NIL
HORIZONTAL

CHOOSER
13
108
152
153
country
country
"Vietnam" "Cambodia" "None"
1

SLIDER
13
215
185
248
heterogeneityIndex
heterogeneityIndex
0
2
0.44038794876162135
0.01
1
NIL
HORIZONTAL

BUTTON
14
60
150
93
NIL
setup-monte-carlo
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
756
113
892
146
displayImages
displayImages
1
1
-1000

PLOT
407
349
607
499
Heavy Spray Patches
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count patches with [choice = 3]"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

crop-base
false
0
Polygon -10899396 true false 240 285 285 225 285 165
Polygon -10899396 true false 225 285 210 135 180 75 180 240
Polygon -10899396 true false 225 105 240 105 240 285 225 285
Polygon -10899396 true false 90 270 45 60 45 210
Polygon -10899396 true false 105 270 165 180 165 90 120 180
Polygon -10899396 true false 90 60 90 285 105 285 105 60
Circle -1184463 true false 54 54 42
Circle -1184463 true false 99 69 42
Circle -1184463 true false 54 99 42
Circle -1184463 true false 99 114 42
Circle -1184463 true false 54 144 42
Circle -1184463 true false 99 159 42
Circle -1184463 true false 54 189 42
Circle -1184463 true false 84 24 42
Circle -1184463 true false 234 99 42
Circle -1184463 true false 189 114 42
Circle -1184463 true false 204 69 42
Circle -1184463 true false 234 144 42
Circle -1184463 true false 189 159 42
Circle -1184463 true false 234 189 42
Circle -1184463 true false 189 204 42

crop-spray-heavy
false
0
Polygon -10899396 true false 240 285 285 225 285 165
Polygon -10899396 true false 225 285 210 135 180 75 180 240
Polygon -10899396 true false 225 105 240 105 240 285 225 285
Polygon -10899396 true false 90 270 45 60 45 210
Polygon -10899396 true false 105 270 165 180 165 90 120 180
Polygon -10899396 true false 90 60 90 285 105 285 105 60
Circle -1184463 true false 54 54 42
Circle -1184463 true false 99 69 42
Circle -1184463 true false 54 99 42
Circle -1184463 true false 99 114 42
Circle -1184463 true false 54 144 42
Circle -1184463 true false 99 159 42
Circle -1184463 true false 54 189 42
Circle -1184463 true false 84 24 42
Circle -1184463 true false 234 99 42
Circle -1184463 true false 189 114 42
Circle -1184463 true false 204 69 42
Circle -1184463 true false 234 144 42
Circle -1184463 true false 189 159 42
Circle -1184463 true false 234 189 42
Circle -1184463 true false 189 204 42
Circle -2674135 true false 15 135 30
Circle -2674135 true false 45 105 30
Circle -2674135 true false 90 90 30
Circle -2674135 true false 90 180 30
Circle -2674135 true false 180 30 30
Circle -2674135 true false 45 165 30
Circle -2674135 true false 135 15 30
Circle -2674135 true false 255 135 30
Circle -2674135 true false 180 90 30
Circle -2674135 true false 135 195 30
Circle -2674135 true false 45 60 30
Circle -2674135 true false 60 210 30
Circle -2674135 true false 225 165 30
Circle -2674135 true false 90 30 30
Circle -2674135 true false 75 135 30
Circle -2674135 true false 90 240 30
Circle -2674135 true false 135 75 30
Circle -2674135 true false 225 105 30
Circle -2674135 true false 135 135 30
Circle -2674135 true false 195 135 30
Circle -2674135 true false 210 210 30
Circle -2674135 true false 180 180 30
Circle -2674135 true false 225 60 30
Circle -2674135 true false 180 240 30
Circle -2674135 true false 135 255 30

crop-spray-light
false
0
Polygon -10899396 true false 240 285 285 225 285 165
Polygon -10899396 true false 225 285 210 135 180 75 180 240
Polygon -10899396 true false 225 105 240 105 240 285 225 285
Polygon -10899396 true false 90 270 45 60 45 210
Polygon -10899396 true false 105 270 165 180 165 90 120 180
Polygon -10899396 true false 90 60 90 285 105 285 105 60
Circle -1184463 true false 54 54 42
Circle -1184463 true false 99 69 42
Circle -1184463 true false 54 99 42
Circle -1184463 true false 99 114 42
Circle -1184463 true false 54 144 42
Circle -1184463 true false 99 159 42
Circle -1184463 true false 54 189 42
Circle -1184463 true false 84 24 42
Circle -1184463 true false 234 99 42
Circle -1184463 true false 189 114 42
Circle -1184463 true false 204 69 42
Circle -1184463 true false 234 144 42
Circle -1184463 true false 189 159 42
Circle -1184463 true false 234 189 42
Circle -1184463 true false 189 204 42
Circle -2674135 true false 90 90 30
Circle -2674135 true false 90 180 30
Circle -2674135 true false 135 195 30
Circle -2674135 true false 180 90 30
Circle -2674135 true false 135 75 30
Circle -2674135 true false 195 135 30
Circle -2674135 true false 135 135 30
Circle -2674135 true false 75 135 30
Circle -2674135 true false 180 180 30

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

non-crop
false
0
Polygon -6459832 true false 180 255 120 195 135 195 165 225 135 135 150 150 165 210 195 105 210 120 180 195 210 165 210 180 180 210
Polygon -6459832 true false 60 210 105 255 75 120 60 120 90 225 60 195
Circle -10899396 true false 26 86 67
Circle -10899396 true false 116 101 67
Circle -10899396 true false 163 58 92
Circle -10899396 true false 45 180 30
Circle -10899396 true false 99 159 42
Circle -10899396 true false 195 150 30
Circle -10899396 true false 146 176 67
Polygon -13840069 true false 135 255 105 45 75 30 105 105 135 255
Polygon -13840069 true false 255 240 270 60 240 30 240 240
Polygon -13840069 true false 135 255 45 60 30 45 120 240
Polygon -13840069 true false 135 255 45 15 60 15 120 210
Polygon -6459832 true false 195 105 165 30 180 90 135 75 180 105
Circle -10899396 true false 144 9 42
Circle -10899396 true false 120 60 30

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person farmer
false
0
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -1 true false 60 195 90 210 114 154 120 195 180 195 187 157 210 210 240 195 195 90 165 90 150 105 150 150 135 90 105 90
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -13345367 true false 120 90 120 180 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 180 90 172 89 165 135 135 135 127 90
Polygon -6459832 true false 116 4 113 21 71 33 71 40 109 48 117 34 144 27 180 26 188 36 224 23 222 14 178 16 167 0
Line -16777216 false 225 90 270 90
Line -16777216 false 225 15 225 90
Line -16777216 false 270 15 270 90
Line -16777216 false 247 15 247 90
Rectangle -6459832 true false 240 90 255 300

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Sensitivity Analysis" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup-monte-carlo</setup>
    <go>go</go>
    <exitCondition>endRun = true</exitCondition>
    <metric>heterogeneityIndex</metric>
    <metric>sizeStdDev</metric>
    <metric>sizeRange</metric>
    <metric>largestPlot</metric>
    <metric>coeffVariation</metric>
    <metric>giniCoeff</metric>
    <metric>trustIndexMean</metric>
    <metric>carePatchMean</metric>
    <metric>careFarmerMean</metric>
    <metric>careOthersMean</metric>
    <metric>initialExperience</metric>
    <metric>nchPercent</metric>
    <metric>memory</metric>
    <metric>percentageSD</metric>
    <metric>listFarmerScore</metric>
    <metric>listNetProduction</metric>
    <metric>listNCHCells</metric>
    <metric>listNCHStrategy</metric>
    <metric>listNCHBenefit</metric>
    <metric>listDeviation</metric>
    <metric>listHSCells</metric>
    <metric>listStrategySwitches</metric>
    <metric>listNCHSwitches</metric>
    <metric>listPopularStrategy</metric>
    <metric>listNCH_NonNCH</metric>
    <metric>listNCH_NCH</metric>
    <metric>listNCHBenefit_NonNCH</metric>
    <metric>listNCHBenefit_NCH</metric>
    <steppedValueSet variable="numFarms" first="4" step="1" last="50"/>
  </experiment>
  <experiment name="farm_size_analysis_single" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>endRun = true</exitCondition>
    <metric>sizeStdDev</metric>
    <metric>sizeRange</metric>
    <metric>largestPlot</metric>
    <metric>coeffVariation</metric>
    <metric>giniCoeff</metric>
    <metric>listFarmerScore</metric>
    <metric>listNetProduction</metric>
    <metric>listNCHCells</metric>
    <metric>listNCHStrategy</metric>
    <metric>listNCHBenefit</metric>
    <metric>listDeviation</metric>
    <metric>listHSCells</metric>
    <metric>listNCHEdge</metric>
    <metric>listStrategySwitches</metric>
    <metric>listNCHSwitches</metric>
    <metric>listPopularStrategy</metric>
    <metric>listNCHBenefit_NonNCH</metric>
    <metric>listNCHBenefit_NCH</metric>
    <enumeratedValueSet variable="country">
      <value value="&quot;Vietnam&quot;"/>
      <value value="&quot;Cambodia&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numFarms" first="4" step="1" last="50"/>
    <enumeratedValueSet variable="sideLength">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="heterogeneityIndex">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uniformPlot">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="het_analysis_single" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>endRun = true</exitCondition>
    <metric>sizeStdDev</metric>
    <metric>sizeRange</metric>
    <metric>largestPlot</metric>
    <metric>coeffVariation</metric>
    <metric>giniCoeff</metric>
    <metric>listFarmerScore</metric>
    <metric>listNetProduction</metric>
    <metric>listNCHCells</metric>
    <metric>listNCHStrategy</metric>
    <metric>listNCHBenefit</metric>
    <metric>listDeviation</metric>
    <metric>listHSCells</metric>
    <metric>listStrategySwitches</metric>
    <metric>listNCHSwitches</metric>
    <metric>listPopularStrategy</metric>
    <metric>listNCH_NonNCH</metric>
    <metric>listNCH_NCH</metric>
    <metric>listNCHBenefit_NonNCH</metric>
    <metric>listNCHBenefit_NCH</metric>
    <enumeratedValueSet variable="country">
      <value value="&quot;Vietnam&quot;"/>
      <value value="&quot;Cambodia&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numFarms" first="4" step="1" last="50"/>
    <steppedValueSet variable="heterogeneityIndex" first="0" step="0.05" last="0.5"/>
    <enumeratedValueSet variable="sidelength">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uniformPlot">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="farm_size_analysis_means" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>endRun = true</exitCondition>
    <metric>sizeStdDev</metric>
    <metric>sizeRange</metric>
    <metric>largestPlot</metric>
    <metric>coeffVariation</metric>
    <metric>giniCoeff</metric>
    <metric>listFarmerScore</metric>
    <metric>listNetProduction</metric>
    <metric>listNCHCells</metric>
    <metric>listNCHStrategy</metric>
    <metric>listNCHBenefit</metric>
    <metric>listDeviation</metric>
    <metric>listHSCells</metric>
    <metric>listNCHEdge</metric>
    <metric>listStrategySwitches</metric>
    <metric>listNCHSwitches</metric>
    <metric>listPopularStrategy</metric>
    <metric>listNCHBenefit_NonNCH</metric>
    <metric>listNCHBenefit_NCH</metric>
    <enumeratedValueSet variable="country">
      <value value="&quot;Vietnam&quot;"/>
      <value value="&quot;Cambodia&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numFarms" first="4" step="1" last="50"/>
    <enumeratedValueSet variable="sideLength">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="heterogeneityIndex">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="uniformPlot">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@

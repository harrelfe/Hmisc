##' Data from The Analysis of Biological Data by Shitlock and Schluter
##'
##' Fetches csv files for exercises in the book
##' @title getabd
##' @param name name of dataset to fetch.  Omit to get a data table listing all available datasets.
##' @param lowernames set to `TRUE` to change variable names to lower case
##' @param allow set to `NULL` to convert underscores in variable names to periods
##' @return data frame with attributes `label` and `url`
##' @author Frank Harrell
##' @md
##' @export
getabd <- function(name='', lowernames=FALSE, allow='_') {
# Source: https://biostat.app.vumc.org/MsciBiostatIIDatasets
  
abd <- read.csv(textConnection('
x1|name|label|url|x2
|02-e-2a|Deaths from tigers|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02e2aDeathsFromTigers.csv|
|02-e-2b|Desert bird abundance|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02e2bDesertBirdAbundance.csv|
|02-e-3a|Bird malaria|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02e3aBirdMalaria.csv|
|02-e-3b|Guppy father and son comparison|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02e3bGuppyFatherSonAttractiveness.csv|
|02-e-3c|Human hemoglobin and elevation|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02e3cHumanHemoglobinElevation.csv|
|02-e-4a|Measles outbreaks|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02e4aMeaslesOutbreaks.csv|
|02-q-05|Fish fry survival|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q05FrySurvival.csv|
|02-q-06|Endangered species|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q06EndangeredSpecies.csv|
|02-q-07|Famine and schizophrenia|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q07FamineSchizophrenia.csv|
|02-q-12|Convictions and income|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q12ConvictionsAndIncome.csv|
|02-q-17|Anemone personality|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q17AnemonePersonality.csv|
|02-q-19|Firefly spermatophore mass|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q19FireflySpermatophoreMass.csv|
|02-q-22|Criminal convictions|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q22CriminalConvictions.csv|
|02-q-26|Neotropical tree photosynthesis|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q26NeotropicalTreePhotosynthesis.csv|
|02-q-28|Sneakers and cannibalism|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q28SneakerCannibalism.csv|
|02-q-32|Toxoplasma and accidents|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q32ToxoplasmaAccidents.csv|
|02-q-33|ADHD and birth month|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q33BirthMonthADHD.csv|
|02-q-35|Food intake and lifespan|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q35FoodReductionLifespan.csv|
|02-q-37|Number of hurricanes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q37Hurricanes.csv|
|03-e-1|Gliding snakes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03e1GlidingSnakes.csv|
|03-t-1|Criminal convictions|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02q12and22CriminalConvictions.csv|
|03-e-2|Amputated spiders|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03e2SpiderAmputation.csv|
|03-e-3|Stickleback lateral plates|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03e3SticklebackPlates.csv|
|03-q-01|Systolic blood pressure|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03q01SystolicBP.csv|
|03-q-02|Systolic blood pressure, larger sample|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03q02SystolicBP.csv|
|03-q-04|Kenya finches|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03q04KenyaFinches.csv|
|03-q-09|Rigor mortis|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03q09Rigormortis.csv|
|03-q-14|Vasopressin voles|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03q14VasopressinVoles.csv|
|03-q-15|Diet breadth El Verde|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03q15DietBreadthElVerde.csv|
|03-q-19|Sparrow reprodution|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03q19SparrowReproductiveSuccess.csv|
|03-q-21|Yeast mutant growth rate|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03q21YeastMutantGrowth.csv|
|03-q-22|Zebra fish boldness|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03q22ZebraFishBoldness.csv|
|03-q-28|Sea urchin bindin|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03q28SeaUrchinBindin.csv|
|04-e-1|Human gene lengths|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter04/chap04e1HumanGeneLengths.csv|
|04-q-07|Firefly flashes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter04/chap04q07FireflyFlash.csv|
|04-q-09|Gene regulatory networks|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter04/chap04q09NumberGenesRegulated.csv|
|04-q-18|Corpseflowers|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter04/chap04q18Corpseflowers.csv|
|07-e-2|Sex and the X chromosome|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07e2SexAndX.csv|
|07-e-3|Sex of Radiologists offspring|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07e3RadiologistOffspringSex.csv|
|07-q-2|Which ear for questions|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q2QuestionEar.csv|
|07-q-03|Honesty wallets|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q03LostWallets.csv|
|07-q-04|John Wayne|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q04GenghisKhanCancer.csv|
|07-q-05|Cocaine on dollars|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q05CocaineDollars.csv|
|07-q-07|Eyelash mites|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q07FollicleMites.csv|
|07-q-08|Garden spider cannibalism|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q08GardenSpiderCannibalism.csv|
|07-q-09|Range shifts with climate change|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q09ClimateRangeShifts.csv|
|07-q-11|Mouse fetal testosterone|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q11FetalTestosterone.csv|
|07-q-14|Christmas deaths|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q14WaitingForChristmas.csv|
|07-q-15|Plant chemical defenses|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q15Latex.csv|
|07-q-16|Heroin on dollars|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q16HeroinDollars.csv|
|07-q-18|Selective looking|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q18SelectiveLooking.csv|
|07-q-19|Food injuries|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q19CutsFromFood.csv|
|07-q-20|Finding love through pets|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q20LoveThroughPets.csv|
|07-q-23|Butter side down|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q23ButterSideDown.csv|
|07-q-23|Surgical site infections|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q23SurgicalSiteInfections.csv|
|07-q-25|Helping at the nest|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q25HelpingAtNest.csv|
|07-q-26|Dog food pate |http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q26DogFoodPate.csv|
|07-q-27|Moods and seasons|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q27SuicideSeason.csv|
|07-q-28|Road kill|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q28RoadKill.csv|
|07-q-30|Catfish hunting pigeons|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07q30CatfishHunting.csv|
|08-e-1|Days of birth|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08e1DayOfBirth.csv|
|08-e-4|Gene content of chromosomes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08e4XGeneContent.csv|
|08-e-5|Number of boys in two-child families|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08e5NumberOfBoys.csv|
|08-e-6|Mass extinctions|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08e6MassExtinctions.csv|
|08-q-1|Testing spatial computer program|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q1testingComputerProgram.csv|
|08-q-02|Shad parasites|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q02ShadParasites.csv|
|08-q-03|Resistance to bacteria|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q03F2Resistance.csv|
|08-q-05|World Cup|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q05WorldCup.csv|
|08-q-6|Coin flipping|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q6CoinFlipping.csv|
|08-q-12|Spirit bear genetics|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q12SpiritBearGenetics.csv|
|08-q-14|Bird window crashes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q14BirdWindowCrash.csv|
|08-q-14|Cavalry|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q14Cavalry.csv|
|08-q-16|Hospital weekend or weekday|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q16HospitalAdmission.csv|
|08-q-16|Truffles|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q16Truffles.csv|
|08-q-18|Anemonefish sexes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q18AnemonefishSexes.csv|
|08-q-19|Hurricanes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q19Hurricanes.csv|
|08-q-21|Falling cats|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q21FallingCatsByMonth.csv|
|08-q-24|Dodder growth orientation|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q24DodderGrowth.csv|
|08-q-25|Dodder and &#945;-pinene|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q25DodderWithPinene.csv|
|09-e-2|Aspirin vs. cancer|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09e2AspirinCancer.csv|
|09-e-3|Toxoplasmosis|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09e3ToxoplasmaAndAccidents.csv|
|09-e-3|Parasite control of behavior|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09e3ParasiteBrainWarp.csv|
|09-e-4|Vampire bats and estrous|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09e4VampireBites.csv|
|09-q-01|Coffee and cancer|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q01CoffeeAndCancer.csv|
|09-q-02|Divorce after diagnosis|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q02DiseaseAndDivorce.csv|
|09-q-04|Pigeon rumps|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q04PigeonRumps.csv|
|09-q-05|Malaria transmission|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q05MalariaAndMosquitoFeeding.csv|
|09-q-06|Redback spider sexual cannibalism|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q06RedbackSpiderCannibalism.csv|
|09-q-07|Reed frogs and fire|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q07FrogsFire.csv|
|09-q-08|Fish sex change|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q08FishSexChange.csv|
|09-q-09|TV violence|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q09TVAndAggression.csv|
|09-q-10|Drinking and heart disease|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q10AlcoholHeartAttacks.csv|
|09-q-11|Postnatal depression|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q11PostnatalDepression.csv|
|09-q-12|Migraines with auras|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q12MigraineShunts.csv|
|09-q-14|Mediterranean diet|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q14MedDiet.csv|
|09-q-16|Prairie dog multiple mating|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q16PrairieDogMultipleMating.csv|
|09-q-17|Intuition about lying|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q17LyingIntuition.csv|
|09-q-19|Contagious yawning|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q19Yawning.csv|
|09-q-20|Daycare and leukemia|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q20ALLDaycare.csv|
|09-q-21|Blue termites|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q21BlueTermites.csv|
|09-q-22|Self recognition|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q22SelfRecognition.csv|
|09-q-24|Heat and sterility|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q24HeatSterility.csv|
|09-q-26|Just because|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q26Because.csv|
|09-q-27|Bereavement and health|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q27WidowHealth.csv|
|09-q-29|Denomination effect|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q29Denomination.csv|
|09-q-30|Firearms and suicide|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q30FirearmsSuicide.csv|
|09-q-31|Nectar spurs|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q31NectarSpurs.csv|
|09-q-33|Kuru|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q33Kuru.csv|
|10-e-6|Deaths from Spanish flu 1918|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter10/chap10e6DeathsSpanishFlu1918.csv|
|10-q-17|Tree growth rate|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter10/chap10q17TreeGrowthRate.csv|
|10-q-19|Health expenditures|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter10/chap10q19HealthExpenditures.csv|
|11-e-2|Stalk-eyed flies|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11e2Stalkies.csv|
|11-e-3|Human body temperature|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11e3Temperature.csv|
|11-q-01|Range shifts with climate change|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11q01RangeShiftsWithClimateChange.csv|
|11-q-06|Wolf jaws|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11q06WolfTeeth.csv|
|11-q-13|Syrup swimming|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11q13SyrupSwimming.csv|
|11-q-16|Dolphins clockwise|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11q16DolphinsClockwise.csv|
|11-q-17|Koala bellows|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11q17KoalaBellows.csv|
|11-q-18|Stickleback preferences|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11q18SticklebackPreference.csv|
|11-q-21|Hurricanes and soil lead|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11q21SoilLeadAndHurricanes.csv|
|11-q-22|Hurricanes and blood lead|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11q22BloodLeadKatrina.csv|
|11-q-24|Walking in circles|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11q24WalkingInCircles.csv|
|11-q-25|Sloth inner ears|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter11/chap11q25SlothEars.csv|
|12-e-2|Blackbird testosterone|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12e2BlackbirdTestosterone.csv|
|12-e-3|Horned lizards|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12e3HornedLizards.csv|
|12-e-4|Brook trout vs. salmon|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12e4ChinookWithBrookTrout.csv|
|12-q-01|Death and taxes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q01DeathAndTaxes.csv|
|12-q-6|Testes size|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q6TestesSize.csv|
|12-q-09|Cichlids|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q09Cichlids.csv|
|12-q-10|"Will"s and presidents|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q10WillsPresidents.csv|
|12-q-12|Ostrich temperatures|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q12OstrichTemp.csv|
|12-q-14|Iguanas|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q14Iguanas.csv|
|12-q-16|Mosquitoes and beer|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q16BeerAndMosquitoes.csv|
|12-q-17|HIV immunity|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q17HIVAntibody.csv|
|12-q-18|Stalkie eyesmall|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q18StalkieEyespan.csv|
|12-q-20|Electric fish|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q20ElectricFish.csv|
|12-q-24|Weddell seal dives|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q24WeddellSeals.csv|
|12-q-26|Hyena giggles|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q26HyenaGiggles.csv|
|12-q-31|Rat reciprocity|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q31RatReciprocity.csv|
|12-q-32|Glassware|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q32BeerGlassShape.csv|
|12-q-33|Spinocerebellar ataxia |http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter12/chap12q33SpinocerebellarAtaxia.csv|
|13-e-1|Marine reserves|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13e1MarineReserve.csv|
|13-e-4|Sexual Conflict|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13e4SexualConflict.csv|
|13-e-5|Sagebrush crickets|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13e5SagebrushCrickets.csv|
|13-q-01|Country Health Expenditure|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q01CountryHealthExpenditure.csv|
|13-q-02|Goldeneye parasitism|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q02GoldeneyeParasitism.csv|
|13-q-03|Recycling|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q03Recycling.csv|
|13-q-07|Senicio seed set |http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q07SeedSet.csv|
|13-q-08|Lion infanticide|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q08Lions.csv|
|13-q-09|Newts|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q09Newts.csv|
|13-q-13|Worm sperm|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q13WormSperm.csv|
|13-q-14|Dioecy|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q14Dioecy.csv|
|13-q-15|Mosquitoes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q15Mosquitoes.csv|
|13-q-16|Beetles and angiosperms|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q16BeetleAngiosperms.csv|
|13-q-18|Zebra finches|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q18ZebraFinches.csv|
|13-q-19|Sports vs. Biology, Sex partners|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q19SportsVsBiology.csv|
|13-q-20|Salmon color|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q20SalmonColor.csv|
|13-q-21|Stress and incompatible mates|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q21StressAndIncompatibleMates.csv|
|13-q-25| Clearcuts|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q25Clearcuts.csv|
|13-q-26| Zebra finch beaks|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q26ZebraFinchBeaks.csv|
|13-q-27|Vuvuzela|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q27Vuvuzela.csv|
|19-q-28|Pseudoscorpions|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap19q28Pseudoscorpions.csv|
|13-q-29|Army ants and silverfish|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q29ArmyAntSilverfish.csv|
|13-q-30|Wolbachia, mosquitoes, and dengue|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q30WolbachiaAndDengue.csv|
|13-q-32|Gut flora and autoimmune disease|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter13/chap13q32GutFloraAutoimmune.csv|
|15-e-1|Knees who say night|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15e1KneesWhoSayNight.csv|
|15-e-6|Walking sticks|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15e6WalkingStickFemurs.csv|
|15-q-01|Caffeine and nectar|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q01HoneybeeCaffeine.csv|
|15-q-03|Exam repeatability|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q03ExamGrades.csv|
|15-q-04|Plant population persistence|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q04PlantPopulationPersistence.csv|
|15-q-08|Disorders|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q08DisordersAndGeneExpression.csv|
|15-q-11|Dung beetles|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q11DungBeetleCondition.csv|
|15-q-13|Ricketsiella and aphid Color|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q13RicketsiellaColor.csv|
|15-q-14|Tsetse learning|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q14TsetseLearning.csv|
|15-q-15|Eelgrass|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q15EelgrassGenotypes.csv|
|15-q-17|Daphnia resistance|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q17DaphniaResistance.csv|
|15-q-20|Nemotode lifespan|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q20NematodeLifespan.csv|
|15-q-22|Walking stick heads|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q22WalkingStickHeads.csv|
|15-q-23|Lodgepole pine cones|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q23LodgepolePineCones.csv|
|15-q-24|Lupus mice|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q24LupusProneMice.csv|
|15-q-25|Lizard sprint speed|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q25LizardSprintSpeed.csv|
|15-q-26|Mosquito poisoning|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q26MalariaFungusVenom.csv|
|15-q-27|Meaningless math|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q27NonsenseMathEffect.csv|
|15-q-28|Fly self-medication|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q28FlySelfMedication.csv|
|15-q-30|Fiddler crab claw-fans|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q30FiddlerCrabFans.csv|
|15-q-32|Hippocampal volume loss|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter15/chap15q32HippocampalVolumeRatio.csv|
|16-e-1|Flipping birds|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16e1FlippingBird.csv|
|16-e-2|Wolf inbreeding|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16e2InbreedingWolves.csv|
|16-e-5|Indian rope trick|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16e5IndianRopeTrick.csv|
|16-q-01|Hyena age|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q01Hyena%20GigglesAndAge.csv|
|16-q-03|TB resistance|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q03TBResistance.csv|
|16-q-05|Godwits|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q05GodwitArrivalDates.csv|
|16-q-10|Earwig forceps|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q10EarwigForceps.csv|
|16-q-12|Cricket immunity|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q12CricketImmunitySpermViability.csv|
|16-q-13|Lefthandedness and violence|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q13LefthandednessAndViolence.csv|
|16-q-14|Telomeres and stress|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q14TelomeresAndStress.csv|
|16-q-15|Language and grey matter|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q15LanguageGreyMatter.csv|
|16-q-16|Green space|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q16GreenSpaceBiodiversity.csv|
|16-q-18|Salmon and salmonberries|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q18SalmonSalmonberries.csv|
|16-q-19|Liver preparation|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q19LiverPreparation.csv|
|16-q-20|Sleep and performance|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q20SleepAndPerformance.csv|
|16-q-22|Trillium|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q22TrilliumRecruitment.csv|
|16-q-23|Cocaine|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q23CocaineHigh.csv|
|16-q-24|Extrovertism and neuroticism|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q24ExtroversionNeuroticism.csv|
|16-q-26|Chocolate and Nobel Prizes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q26ChocolateAndNobel.csv|
|16-q-27|Antibody tumor screening|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter16/chap16q27AntibodyTumorScreening.csv|
|17-e-1|Lion ages|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17e1LionNoses.csv|
|17-e-3|Plant species and biomass|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17e3PlantDiversityAndStability.csv|
|17-e-8|Shrinking seals|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17e8ShrinkingSeals.csv|
|17-f-5_2|Junco outlier|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17f5_2JuncoOutlier.csv|
|17-f-5_3|Desert pool fish species|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17f5_3DesertPoolFish.csv|
|17-f-5_4|Cap color|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17f5_4BlueTitCapColor.csv|
|17-f-5_4|Cockroach neurons|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17f5_4CockroachNeurons.csv|
|17-f-6_3|Iris pollen|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17f6_3IrisPollination.csv|
|17-f-8_1|Iron and plankton growth|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17f8_1IronAndPhytoplanktonGrowth.csv|
|17-f-8_2|Plant species and productivity|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17f8_2PondPlantsAndProductivity.csv|
|17-f-9_1|Guppy temperature mortality|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17f9_1GuppyColdDeath.csv|
|17-q-01|Faces and aggression|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q01FacesAndPenalties.csv|
|17-q-06|Zoo mortality|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q06ZooMortality.csv|
|17-q-07|Progesterone|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q07ProgesteroneExercise.csv|
|17-q-10|Hybrid pollen sterility|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q10HybridPollenSterility.csv|
|17-q-11|Rattlensake digestion|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q11RattlesnakeDigestion.csv|
|17-q-12|Lizard bites|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q12LizardBite.csv|
|17-q-14|Hypoxanthine|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q14HypoxanthineTimeOfDeath.csv|
|17-q-15|Social spiders|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q15SocialSpiderColonies.csv|
|17-q-17|Earthworms and nitrogen|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q17EarthwormsAndNitrogen.csv|
|17-q-18|Cypress respiration|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q18CypressRespiration.csv|
|17-q-19|Grassland nutrients|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q19GrasslandNutrientsPlantSpecies.csv|
|17-q-20|Primate mass and metabolic rate|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q20PrimateMassMetabolicRate.csv|
|17-q-22|Flycatcher patch|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q22FlycatcherPatch.csv|
|17-q-23|Tree seedlings and sunflecks|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q23TreeSeedlingsAndSunflecks.csv|
|17-q-24|Penguin treadmill|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q24PenguinTreadmill.csv|
|17-q-25|Beetle wings and horns|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q25BeetleWingsAndHorns.csv|
|17-q-26|Song of extinct katydid|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q26SongExtinctKatydid.csv|
|17-q-27|Daphnia parasites|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q27DaphniaParasiteLongevity.csv|
|17-q-28|Brain metabolism|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q28BrainMetabolism.csv|
|17-q-29|Mosquito bites|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q29DEETMosquiteBites.csv|
|17-q-30|Nuclear teeth|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q30NuclearTeeth.csv|
|17-q-31|Last supper portion sizes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q31LastSupperPortionSize.csv|
|17-q-32|Coral snake mimics|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q32CoralSnakeMimics.csv|
|17-q-33|Egg laying date and climate change|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q33EggLayingMismatch.csv|
|17-q-34|Fathers age and mutations|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q34FatherAgeMutations.csv|
|17-q-35|Anthrax mortality|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q35AnthraxMortality.csv|
|17-q-36|Stork stress|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter17/chap17q36StorkStress.csv|
|18-e-2|Zooplanckton|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter18/chap18e2ZooplanktonDepredation.csv|
|18-e-3|Intertidal algae|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter18/chap18e3IntertidalAlgae.csv|
|18-e-4|Mole rats|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter18/chap18e4MoleRatLayabouts.csv|
|18-q-06|Hippocampus lesions|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter18/chap18q06HippocampusLesions.csv|
|18-q-07|SP and female lifespan|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter18/chap18q07flyLifeSpan.csv|
|18-q-08|Bee genes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter18/chap18q08BeeGeneExpression.csv|
|18-q-11|Mouse empathy|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter18/chap18q11MouseEmpathy.csv|
|18-q-12|Neanderthal brains|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter18/chap18q12NeanderthalBrainSize.csv|
|18-q-15|Larval fish|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter18/chap18q15ExploitedLarvalFish.csv|
|18-q-18|Opsin expression|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter18/chap18q18OpsinExpression.csv|
|19-e-1|Two-digit "psychic"|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter19/chap19e1TwoDigitNumbers.csv|
|19-e-2|Chimp language centers|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter19/chap19e2ChimpBrains.csv|
|19-q-11|Snail love darts|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter19/chap19q11LoveDarts.csv|
|19-q-13|Leaf bacteria|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter19/chap19q13BacteriaPerLeaf.csv|
|19-q-15|Speed of light|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter19/chap19q15SpeedOfLight.csv|
|20-e-3|Unruly passengers|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter20/chap20e3UnrulyPassengers.csv|
|20-e-4|Conservation scoop|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter20/chap20e4ConservationScoop.csv|
|20-q-07|Yeast regulatory genes|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter20/chap20q07YeastRegulatoryGenes.csv|
|20-q-08|Silverswords|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter20/chap20q08SilverswordWaitingTimes.csv|
|20-q-15|Vole dispersal|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter20/chap20q15VoleDispersal.csv|
|20-q-18|Bee lifespans|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter20/chap20q18BeeLifespans.csv|
|21-p-3|Selection|http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter21/selection.csv|
'), sep='|')

nam <- abd$name
lab <- abd$label
url <- abd$url

if(name == '') return(data.frame(name=nam, label=lab))

i <- nam == name
if(! any(i)) stop(paste('No dataset named', name))
cat('Retrieving ', lab[i], ' dataset\n', sep='') 
w <- csv.get(url[i], lowernames=lowernames, allow=allow)
attr(w, 'label') <- lab[i]
attr(w, 'url'  ) <- url[i]  
w
}

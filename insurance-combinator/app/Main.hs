module Main (main) where

import Data.List 
import System.IO 
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Char(isSpace)
import Control.Monad (when)



type InsuranceCategory = [(String, Int)]

-------------------------------------------common combinators-------------------------------


inflation :: Double
inflation = 0.85


duration :: Double -> Double
duration x = x


ageCategory :: InsuranceCategory
ageCategory = [("A", 2), ("B", 4), ("C", 6), ("D", 8), ("E", 10), ("F", 15), ("Z", 0)]

age :: Int -> InsuranceCategory -> Maybe Int
age ageInput ageCategory = lookup (ageCategoryRange ageInput ageCategory) ageCategory

ageCategoryRange :: Int -> InsuranceCategory -> String
ageCategoryRange ageInput ageCategory =
    let categoryValue = (ageInput + 19) `div` 20
    in fst $ ageCategory !! (categoryValue - 1)      

medicalHistoryHealth :: InsuranceCategory
medicalHistoryHealth = [("cancer", 50), ("heart disease", 30), ("diabetes", 20), ("stroke", 20), 
                  ("kidney disease", 10), ("liver disease", 10), ("lung disease", 10),
                  ("neurological disorder", 10), ("mental health disorder", 10),
                  ("autoimmune disease", 10), ("high blood pressure", 10), ("high cholesterol", 10),
                  ("arthritis", 5), ("allergies", 5), ("thyroid disorder", 5), ("NA", 0)]

medicalHistoryTravel :: InsuranceCategory
medicalHistoryTravel = [("cancer", 2), ("heart disease", 3), ("diabetes", 2), ("stroke", 4), 
                  ("kidney disease", 1), ("liver disease", 3), ("lung disease", 3),
                  ("neurological disorder", 5), ("mental health disorder", 1),
                  ("autoimmune disease", 1), ("high blood pressure", 5), ("high cholesterol", 5),
                  ("arthritis", 1), ("allergies", 2), ("thyroid disorder", 1), ("NA", 0)]                  

printList :: InsuranceCategory -> IO ()
printList conditionsList = do
    mapM_ (\(index, (ailment, _)) -> putStrLn (show index ++ ". " ++ ailment)) (zip [1..] conditionsList)

getMultipleConditions :: InsuranceCategory -> IO [Int]
getMultipleConditions conditionsList = do
    input <- getLine
    let indices = map (\x -> x - 1) $ map read (splitOn "," input) :: [Int]
    return indices

getMedicalConditionName :: Int -> String
getMedicalConditionName index = fst (medicalHistoryHealth !! index)      

-----------------------------------------health insurance---------------------------------


insuranceCover :: InsuranceCategory
insuranceCover = [("g", 20), ("sp", 40), ("p", 60)]

insuranceTypeCategory :: String -> InsuranceCategory -> Maybe Int
insuranceTypeCategory insuranceTypeInput insuranceCover = lookup insuranceTypeInput insuranceCover 

bmiCategory :: InsuranceCategory
bmiCategory = [("A", 2), ("B", 1), ("C", 4), ("D", 7), ("Z", 0)]

bmiCalculate :: (RealFloat a) => a -> a -> a
bmiCalculate weight height = weight / height ^ 2


bmiWeighting :: (RealFloat a) => a -> InsuranceCategory -> Maybe Int
bmiWeighting bmi' category
    | isNaN bmi' || isInfinite bmi' = lookup "Z" category
    | bmi' <= 18.5 = lookup "A" category
    | bmi' <= 25.0 = lookup "B" category
    | bmi' <= 30.0 = lookup "C" category
    | otherwise   = lookup "D" category


countyList :: InsuranceCategory
countyList = [("Dublin", 60), ("Wicklow", 60), ("Kildare", 60), ("Meath", 60), ("Cork", 60),
                  ("Limerick", 60), ("Galway", 60), ("Louth", 60), ("Carlow", 40), ("Kilkenny", 40),
                  ("Kerry", 40), ("Clare", 40), ("Sligo", 40), ("Mayo", 40), ("Laois", 40), ("Waterford", 40),
                  ("Donegal", 20), ("Leitrim", 20), ("Longford", 20), ("Roscommon", 20), ("Offaly", 20),
                  ("Wexford", 20), ("Westmeath", 20), ("Tipperary", 20), ("Cavan", 20), ("Monaghan", 20)]

getCountyCategory :: String -> InsuranceCategory -> Int
getCountyCategory countyInput cataegoryList = fromMaybe 0 (lookup countyInput countyList)

getHealthInsuranceTypeInput :: IO String
getHealthInsuranceTypeInput = do
    putStrLn "What type of insurance are you looking for? (government/semi-private/private)"
    putStrLn "Type 'g' for government, 'sp' for semi-private OR 'p' for private"
    input <- getLine
    return (trim input)



-----------------------------------Travel Insurance------------------------------------------

travelInsuranceTypes :: InsuranceCategory
travelInsuranceTypes = [("s", 10), ("a", 20), ("b", 13), ("bu", 13), ("f", 10), ("se", 22), ("c", 20)]

travelTypeCategory :: String -> InsuranceCategory -> Maybe Int
travelTypeCategory insuranceTypeInput travelInsuranceTypes = lookup insuranceTypeInput travelInsuranceTypes

getTravelInsuranceTypeInput :: IO String
getTravelInsuranceTypeInput = do
  putStrLn "What type of travel insurance are you looking for?"
  putStrLn "1. Single Trip (s)"
  putStrLn "2. Annual Multi-trip (a)"
  putStrLn "3. Backpacker (b)"
  putStrLn "4. Business (bu)"
  putStrLn "5. Family (f)"
  putStrLn "6. Senior Citizen(se)"
  putStrLn "7. Cruise (c)"
  putStrLn "Enter the initial(s) of your choice:"
  input <- getLine
  return (trim input)
  

travelDestinationList :: InsuranceCategory
travelDestinationList = [("Africe", 10), ("Asia", 8), ("Europe", 12), ("North America", 15), ("South America", 8), 
                    ("Oceania", 11), ("USA", 20)]

getDestinationInput :: IO [Int]
getDestinationInput = do
  putStrLn "Select your travel insurance destination(s):"
  putStrLn "1. Africa"
  putStrLn "2. Asia"
  putStrLn "3. Europe"
  putStrLn "4. North America"
  putStrLn "5. South America"
  putStrLn "6. Oceania"
  putStrLn "7. USA"
  putStrLn "Enter the index(es) of your choice as a comma-separated list:"
  input <- getLine
  let indices = map (\x -> x - 1) $ map read (splitOn "," (trim input)) :: [Int]
  return indices

additionalCoverageOptions :: InsuranceCategory
additionalCoverageOptions = [("sport", 2), ("electronics", 5), ("rentalCar", 7), ("cancelForAnyReason", 10), 
                             ("medicalEvacuation", 9), ("preexistingConditions", 8), ("ad&d", 12), ("None", 0)]

getAdditionalCoverageOptions :: IO [Int]
getAdditionalCoverageOptions = do
  putStrLn "Select additional coverage options:"
  putStrLn "1. Sports and adventure activities"
  putStrLn "2. Electronics coverage"
  putStrLn "3. Rental car excess waiver"
  putStrLn "4. Cancel for any reason"
  putStrLn "5. Medical evacuation and repatriation"
  putStrLn "6. Pre-existing medical conditions"
  putStrLn "7. Accidental Death & Dismemberment (AD&D)"
  putStrLn "8. None"
  putStrLn "Enter the number(s) of any above additional coverage option (comma-seperated list):"
  input <- getLine
  let indices = map (\x -> x - 1) $ map read (splitOn "," (trim input)) :: [Int]
  return indices

------------------------------------------car insurance--------------------------------------

carTypeList :: InsuranceCategory
carTypeList = [("SUV", 70), ("MUV", 60), ("Luxury", 100), ("Sedan", 50), ("Convertible", 90), ("Hatchback", 40)]

getCarTypeName :: Int -> String
getCarTypeName index = fst (carTypeList !! index)      

carUseList :: InsuranceCategory
carUseList = [("Commercial", 30), ("Personal", 20)]

getCarUsageName :: Int -> String
getCarUsageName index = fst (carUseList !! index)      

ageCarCategory :: InsuranceCategory
ageCarCategory = [("A", 2), ("B", 4), ("C", 6), ("D", 8), ("E", 10), ("F", 15), ("Z", 0)]

ageCar :: Double -> InsuranceCategory -> Maybe Int
ageCar ageCarInput ageCarCategory = lookup (ageCarCategoryRange ageCarInput ageCarCategory) ageCarCategory

ageCarCategoryRange :: Double -> InsuranceCategory -> String
ageCarCategoryRange ageCarInput ageCarCategory = 
    case findIndex (\(_, maxAge) -> ageCarInput <= fromIntegral maxAge) ageCarCategory of
      Just index -> fst $ ageCarCategory !! index
      Nothing -> "Z"

------------------------------------error checking functions----------------------------------

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

getTrimmedInput :: Read a => IO a
getTrimmedInput = do
    input <- getLine
    let trimmedInput = trim input
    return (read trimmedInput)


getIntInput :: String -> (Int -> Bool) -> IO Int
getIntInput prompt condition = do
    putStrLn prompt
    input <- getTrimmedInput
    if condition input
        then return input
        else do
            putStrLn "Invalid input, please try again."
            getIntInput prompt condition

getDoubleInput :: String -> (Double -> Bool) -> IO Double
getDoubleInput prompt condition = do
    putStrLn prompt
    input <- getTrimmedInput
    if condition input
        then return input
        else do
            putStrLn "Invalid input, please try again."
            getDoubleInput prompt condition

getCountyInput :: InsuranceCategory -> IO String
getCountyInput countyList= do
    input <- getLine
    let trimmedInput = trim input
    if any (\(county, _) -> county == trimmedInput) countyList
        then return trimmedInput
        else do
            putStrLn "Invalid input, please try again."
            getCountyInput countyList

---------------------------------------------common function for printing---------------------------------

insuranceTypeFullName :: String -> String
insuranceTypeFullName "g" = "government"
insuranceTypeFullName "sp" = "semi-private"
insuranceTypeFullName "p" = "private"
insuranceTypeFullName "s" = "single trip"
insuranceTypeFullName "a" = "annual multi trip"
insuranceTypeFullName "b" = "backpacker"
insuranceTypeFullName "bu" = "business"
insuranceTypeFullName "f" = "family"
insuranceTypeFullName "se" = "senior citizen"
insuranceTypeFullName "c" = "cruise"
insuranceTypeFullName _ = "unknown"

-------------------------------User I/O--------------------------------------------------

getHealthInsuranceInput :: IO (Int, Double, Double, String, String, Double, [Int])
getHealthInsuranceInput = do
    ageInput <- getIntInput "Enter your age:" (\x -> x > 0)

    weightInput <- getDoubleInput "Enter your weight in kg:" (\x -> x > 0)

    heightInput <- getDoubleInput "Enter your height in meters:" (\x -> x > 0)

    putStrLn "Which county do you live in? (Please start the name of the county with a capital letter)"
    countyInput <- getCountyInput countyList

    insuranceTypeInput <- getHealthInsuranceTypeInput 

    putStrLn "Please choose your medical condition(s) from the list below (if any):"
    printList medicalHistoryHealth
    putStrLn "Enter the number(s) of your conditions from the above list (comma-separated list):"
    medicalConditionsList <- getMultipleConditions medicalHistoryHealth

    durationInput <- getDoubleInput "Enter the duration of the health insurance (in years):" (\x -> x > 0)

    return (ageInput, weightInput, heightInput, countyInput, insuranceTypeInput, durationInput, medicalConditionsList)


getTravelInsuranceInput :: IO (Int, Double, [Int], String, [Int], [Int])
getTravelInsuranceInput = do
    ageInput <- getIntInput "Enter your age:" (\x -> x > 0)

    insuranceTypeInput <- getTravelInsuranceTypeInput
    
    destinationList <- getDestinationInput
    
    putStrLn "Please choose your medical condition(s) from the list below (if any):"
    printList medicalHistoryTravel
    putStrLn "Enter the number(s) of your conditions from the above list (comma-separated list):"
    medicalConditionsList <- getMultipleConditions medicalHistoryTravel

    additionalCoverageList <- getAdditionalCoverageOptions
    
    durationInput <- getDoubleInput "Enter the duration of the travel insurance (in days):" (\x -> x > 0)

    return (ageInput, durationInput, medicalConditionsList, insuranceTypeInput, destinationList, additionalCoverageList)

getCarInsuranceInput :: IO (Int, String, [Int], [Int], Double, Double)
getCarInsuranceInput = do
    ageInput <- getIntInput "Enter your age:" (\x -> x > 0)
    
    putStrLn "Enter the county where you want to insure the car (Please start the name of the county with a capital letter):"
    countyInput <- getCountyInput countyList

    putStrLn "Enter the car type from the below list: "
    printList carTypeList
    putStrLn "Please enter the numbers from the above list separated by comma: "
    carTypeInput <- getMultipleConditions carTypeList

    putStrLn "Enter the car use (Commercial or Personal):"
    printList carUseList
    putStrLn "Choose 1 or 2:"
    carUseInput <- getMultipleConditions carUseList

    ageOfVehicle <- getDoubleInput "Enter the age of the vehicle (in years):" (\x -> x >= 0)

    durationInput <- getDoubleInput "Enter the duration of the car insurance (in years):" (\x -> x > 0)

    return (ageInput, countyInput, carTypeInput, carUseInput, ageOfVehicle, durationInput)

------------------------------------------calculation-----------------------------------------------------


calculateHealthTotalWeight :: Int -> Double -> Double -> String -> String -> [Int] -> Int
calculateHealthTotalWeight ageInput weightInput heightInput countyInput insuranceTypeInput medicalConditionsList = 
    fromMaybe 0 ageCategory' 
  + fromMaybe 0 bmiCategory' 
  + countyWeight
  + fromMaybe 0 insuranceTypeCategory'
  + sum medicalConditionWeights
  where
    ageCategory' = age ageInput ageCategory
    bmiValue = bmiCalculate weightInput heightInput
    bmiCategory' = bmiWeighting bmiValue bmiCategory
    countyWeight = getCountyCategory countyInput countyList
    insuranceTypeCategory' = insuranceTypeCategory insuranceTypeInput insuranceCover
    medicalConditionWeights = map (\i -> snd (medicalHistoryHealth !! i)) medicalConditionsList

calculateTravelTotalWeight :: Int -> String -> [Int] -> [Int] -> [Int] -> Int
calculateTravelTotalWeight ageInput insuranceTypeInput medicalConditionsList destinationList additionalCoverageList =
    fromMaybe 0 ageCategory'
  + fromMaybe 0 insuranceTypeCategory'
  + sum medicalConditionWeights
  + sum destinationWeights
  + sum additionalCoverageWeights
  where
    ageCategory' = age ageInput ageCategory
    insuranceTypeCategory' = travelTypeCategory insuranceTypeInput travelInsuranceTypes
    medicalConditionWeights = map (\i -> snd (medicalHistoryTravel !! i)) medicalConditionsList
    destinationWeights = map (\i -> snd (travelDestinationList !! i)) destinationList
    additionalCoverageWeights = map (\i -> snd (additionalCoverageOptions !! i)) additionalCoverageList

calculateCarTotalWeight :: Int -> String -> [Int] -> [Int] -> Double -> Int
calculateCarTotalWeight ageInput countyInput carTypeInput carUseInput ageOfVehicle =
    fromMaybe 0 ageCategory'
  + countyWeight
  + sum carTypeInput'
  + sum carUseInput'
  + fromMaybe 0 ageOfVehicle'
  where
    ageCategory' = age ageInput ageCategory
    countyWeight = getCountyCategory countyInput countyList
    carTypeInput' = map (\i -> snd (carTypeList !! i)) carTypeInput
    carUseInput' = map (\i -> snd (carUseList !! i)) carUseInput
    ageOfVehicle' = ageCar ageOfVehicle ageCarCategory

calculateTotalPriceLong :: Int -> Double -> Int
calculateTotalPriceLong totalWeight insuranceDuration = round (fromIntegral totalWeight * inflation * insuranceDuration)

calculateTotalPriceShort :: Int -> Double -> Int
calculateTotalPriceShort totalweight insuranceDuration = round (fromIntegral totalweight * insuranceDuration)

-----------------------------------------Printing functions------------------------------------------

printHealthUserInputs :: (Int, Double, Double, String, String, Double, [Int]) -> IO ()
printHealthUserInputs (ageInput, weightInput, heightInput, countyInput, insuranceTypeInput, durationInput, medicalConditionsList) = do
    putStrLn " "
    putStrLn "******************************USER INPUTS************************************"
    putStrLn "Your inputs were as follows:"
    putStrLn ("Age: " ++ show ageInput)
    putStrLn ("Weight: " ++ show weightInput ++ " kg")
    putStrLn ("Height: " ++ show heightInput ++ " meters")
    putStrLn ("County: " ++ countyInput)
    putStrLn ("Insurance type: " ++ insuranceTypeFullName insuranceTypeInput)
    putStrLn ("Duration: " ++ show durationInput ++ " years")
    putStrLn ("Medical conditions: " ++ (intercalate ", " (map getMedicalConditionName medicalConditionsList)))


printHealthCompanyInputs :: (Int, Double, Double, String, String, Double, [Int]) -> Int -> Int -> IO ()
printHealthCompanyInputs (ageInput, weightInput, heightInput, countyInput, insuranceTypeInput, durationInput, medicalConditionsList) totalWeightHealth totalPriceHealth = do
    putStrLn " "
    putStrLn "******************************FOR COMPANY USE ONLY************************************"
    putStrLn "The inputs of the user were as follows:"
    
    let ageCategory' = age ageInput ageCategory
    putStrLn ("Age: " ++ show ageInput ++ " (weight: " ++ show (fromMaybe 0 ageCategory') ++ ")")

    putStrLn ("Weight: " ++ show weightInput ++ " kg")
    putStrLn ("Height: " ++ show heightInput ++ " meters")
    
    let bmiValue = bmiCalculate weightInput heightInput
    let finalbmi = round bmiValue
    let bmiCategory' = bmiWeighting bmiValue bmiCategory
    putStrLn ("BMI: " ++ show finalbmi ++ " (weight: " ++ show (fromMaybe 0 bmiCategory') ++ ")")

    putStrLn ("County: " ++ countyInput ++ " (weight: " ++ show (getCountyCategory countyInput countyList) ++ ")")
    
    let insuranceTypeCategory' = insuranceTypeCategory insuranceTypeInput insuranceCover
    putStrLn ("Insurance type: " ++ insuranceTypeFullName insuranceTypeInput ++ " (weight: " ++ show (fromMaybe 0 insuranceTypeCategory') ++ ")")
    
    putStrLn ("Duration: " ++ show durationInput ++ " years")

    let medicalConditionsWithWeights = map (\i -> (getMedicalConditionName i, snd (medicalHistoryHealth !! i))) medicalConditionsList
    putStrLn "Medical conditions:"
    mapM_ (\(condition, weight) -> putStrLn (condition ++ " (weight: " ++ show weight ++ ")")) medicalConditionsWithWeights

    putStrLn ("Total weight: " ++ show totalWeightHealth)
    putStrLn ("Calculation: total weight * duration = " ++ show totalWeightHealth ++ " * " ++ show durationInput ++ " = " ++ show totalPriceHealth ++ " euros")
    
printTravelUserInputs :: (Int, Double, [Int], String, [Int], [Int]) -> IO()    
printTravelUserInputs (ageInput, durationInput, medicalConditionsList, insuranceTypeInput, destinationList, additionalCoverageList) = do
    putStrLn " "
    putStrLn "******************************USER INPUTS************************************"
    putStrLn "Your inputs were as follows:"
    putStrLn ("Age: " ++ show ageInput)
    putStrLn ("Duration: " ++ show durationInput ++ " days")
    putStrLn ("Medical conditions: " ++ (intercalate ", " (map getMedicalConditionName medicalConditionsList)))
    putStrLn ("Insurance type: " ++ insuranceTypeFullName insuranceTypeInput)
    putStrLn ("Destinations: " ++ (intercalate ", " (map (\i -> fst (travelDestinationList !! i)) destinationList)))
    putStrLn ("Additional coverage options: " ++ (intercalate ", " (map (\i -> fst (additionalCoverageOptions !! i)) additionalCoverageList)))

printTravelCompanyInputs :: (Int, Double, [Int], String, [Int], [Int]) -> Int -> Int -> IO()
printTravelCompanyInputs (ageInput, durationInput, medicalConditionsList, insuranceTypeInput, destinationList, additionalCoverageList) totalWeightTravel totalPriceTravel = do
    putStrLn " "
    putStrLn "******************************FOR COMPANY USE ONLY************************************"
    putStrLn "The inputs of the user were as follows:"

    let ageCategory' = age ageInput ageCategory
    putStrLn ("Age: " ++ show ageInput ++ " (weight: " ++ show (fromMaybe 0 ageCategory') ++ ")")
    
    let insuranceTypeCategory' = travelTypeCategory insuranceTypeInput travelInsuranceTypes
    putStrLn ("Insurance type: " ++ insuranceTypeFullName insuranceTypeInput ++ " (weight: " ++ show (fromMaybe 0 insuranceTypeCategory') ++ ")")

    putStrLn ("Duration: " ++ show durationInput ++ " days")

    let medicalConditionsWithWeights = map (\i -> (getMedicalConditionName i, snd (medicalHistoryTravel !! i))) medicalConditionsList
    putStrLn "Medical conditions:"
    mapM_ (\(condition, weight) -> putStrLn (condition ++ " (weight: " ++ show weight ++ ")")) medicalConditionsWithWeights

    let destinationsWithWeights = map (\i -> (fst (travelDestinationList !! i), snd (travelDestinationList !! i))) destinationList
    putStrLn "Destinations:"
    mapM_ (\(destination, weight) -> putStrLn (destination ++ " (weight: " ++ show weight ++ ")")) destinationsWithWeights

    let additionalCoveragesWithWeights = map (\i -> (fst (additionalCoverageOptions !! i), snd (additionalCoverageOptions !! i))) additionalCoverageList
    putStrLn "Additional coverage options:"
    mapM_ (\(coverage, weight) -> putStrLn (coverage ++ " (weight: " ++ show weight ++ ")")) additionalCoveragesWithWeights

    putStrLn ("Total weight: " ++ show totalWeightTravel)
    putStrLn ("Calculation: total weight * duration = " ++ show totalWeightTravel ++ " * " ++ show durationInput ++ " = " ++ show totalPriceTravel ++ " euros")

printCarUserInputs :: (Int, String, [Int], [Int], Double, Double) -> IO()    
printCarUserInputs (ageInput, countyInput, carTypeInput, carUseInput, ageOfVehicle, durationInput) = do
    putStrLn " "
    putStrLn "******************************USER INPUTS************************************"
    putStrLn "Your inputs were as follows:"
    putStrLn ("Age: " ++ show ageInput)
    putStrLn ("Duration: " ++ show durationInput ++ " days")
    putStrLn ("Car Type: " ++ (intercalate ", " (map getCarTypeName carTypeInput)))
    putStrLn ("Car Use: " ++ (intercalate ", " (map getCarUsageName carUseInput)))
    putStrLn ("Age: " ++ show ageOfVehicle ++ " years old")

printCarCompanyInputs :: (Int, String, [Int], [Int], Double, Double) -> Int -> Int -> IO()
printCarCompanyInputs (ageInput, countyInput, carTypeInput, carUseInput, ageOfVehicle, durationInput) totalWeightCar totalPriceCar = do
    putStrLn " "
    putStrLn "******************************FOR COMPANY USE ONLY************************************"
    putStrLn "The inputs of the user were as follows:"

    let ageCategory' = age ageInput ageCategory
    putStrLn ("Age of Insurer: " ++ show ageInput ++ " (weight: " ++ show (fromMaybe 0 ageCategory') ++ ")")
    
    putStrLn ("County: " ++ countyInput ++ " (weight: " ++ show (getCountyCategory countyInput countyList) ++ ")")

    let carType = map (\i -> (getCarTypeName i, snd (carTypeList !! i))) carTypeInput
    putStrLn "Car type(s):"
    mapM_ (\(ctype, weight) -> putStrLn (ctype ++ " (weight: " ++ show weight ++ ")")) carType
    
    let ageCar' = ageCar ageOfVehicle ageCarCategory
    putStrLn ("Car is: " ++ show ageInput ++ " years old" ++ " (weight: " ++ show (fromMaybe 0 ageCar') ++ ")")

    let carUse = map (\i -> (getCarUsageName i, snd (carUseList !! i))) carUseInput
    putStrLn "Car Usage(s):"
    mapM_ (\(cuse, weight) -> putStrLn (cuse ++ " (weight: " ++ show weight ++ ")")) carUse

    putStrLn ("Duration: " ++ show durationInput ++ " years")

    putStrLn ("Total weight: " ++ show totalWeightCar)
    putStrLn ("Calculation: total weight * inflation * duration = " ++ show totalWeightCar ++ " * " ++ show inflation ++ " * " ++ show durationInput ++ " = " ++ show totalPriceCar ++ " euros")

-------------------------------------------------insurance selection------------------------------------
insuranceSelection :: IO ()
insuranceSelection = do
  
  putStrLn "Which insurance are you looking for?"
  putStrLn "1. Health Insurance"
  putStrLn "2. Travel Insurance"
  putStrLn "3. Car Insurance"
  putStrLn "Enter the insurance number of your choice :"
  insuranceChoice <- getLine
  case insuranceChoice of
    "1" -> do
      userInput@(ageInput, weightInput, heightInput, countyInput, insuranceTypeInput, durationInput, medicalConditionsList) <- getHealthInsuranceInput

      let totalWeightHealth = calculateHealthTotalWeight ageInput weightInput heightInput countyInput insuranceTypeInput medicalConditionsList

      let totalPriceHealth = calculateTotalPriceLong totalWeightHealth durationInput

      putStrLn ("Total price of health insurance is: " ++ show totalPriceHealth ++ " euros")
      printHealthUserInputs userInput
      printHealthCompanyInputs userInput totalWeightHealth totalPriceHealth
      return()

    "2" -> do
      travelInsuranceInput@(ageInput, durationInput, medicalConditionsList, insuranceTypeInput, destinationList, additionalCoverageList) <- getTravelInsuranceInput

      let totalWeightTravel = calculateTravelTotalWeight ageInput insuranceTypeInput medicalConditionsList  destinationList additionalCoverageList
      
      let totalPriceTravel = calculateTotalPriceShort totalWeightTravel durationInput

      putStrLn ("Total price of travel insurance is: " ++ show totalPriceTravel ++ " euros")
      printTravelUserInputs travelInsuranceInput
      printTravelCompanyInputs travelInsuranceInput totalWeightTravel totalPriceTravel
      return()

    "3" -> do
      carInsuranceInput@(ageInput, countyInput, carTypeInput, carUseInput, ageOfVehicle, durationInput) <- getCarInsuranceInput

      let totalWeightCar = calculateCarTotalWeight ageInput countyInput carTypeInput carUseInput ageOfVehicle
      
      let totalPriceCar = calculateTotalPriceLong totalWeightCar durationInput

      putStrLn ("Total price of car insurance is: " ++ show totalPriceCar ++ " euros")
      printCarUserInputs carInsuranceInput
      printCarCompanyInputs carInsuranceInput totalWeightCar totalPriceCar
      return()

    _ -> do 
       putStrLn "Invalid choice. Please enter a valid insurance number"
       insuranceSelection

--------------------------------------------main-------------------------------

main :: IO ()
main = do
  putStrLn "Welcome to Insurance Combinator"
  insuranceSelection
    

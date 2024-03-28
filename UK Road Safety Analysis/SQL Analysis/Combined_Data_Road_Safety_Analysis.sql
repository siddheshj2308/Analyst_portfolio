--- Combined Road Safety Data analysis

-- 4 Combine Dataset Using Join 
SELECT 
    Accidents_2015.Accident_Index AS Accident_Index_Accidents,
    Accidents_2015.Location_Easting_OSGR,
    Accidents_2015.Location_Northing_OSGR,
    Accidents_2015.Longitude, 
    Accidents_2015.Latitude, 
    Accidents_2015.Police_Force, 
    Accidents_2015.Accident_Severity, 
    Accidents_2015.Number_of_Vehicles,  
    Accidents_2015.Number_of_Casualties, 
    Accidents_2015.[Date],
    Accidents_2015.Day_of_Week, 
    Accidents_2015.[Time],
    Accidents_2015.Local_Authority_District,
    Accidents_2015.Local_Authority_Highway,
    Accidents_2015.First_Road_Class,  
    Accidents_2015.First_Road_Number,  
    Accidents_2015.Road_Type,
    Accidents_2015.Speed_limit, 
    Accidents_2015.Junction_Detail, 
    Accidents_2015.Junction_Control,
    Accidents_2015.Second_Road_Class,  
    Accidents_2015.Second_Road_Number,  
    Accidents_2015.Pedestrian_Crossing_Human_Control,
    Accidents_2015.Pedestrian_Crossing_Physical_Facilities, 
    Accidents_2015.Light_Conditions,
    Accidents_2015.Weather_Conditions, 
    Accidents_2015.Road_Surface_Conditions, 
    Accidents_2015.Special_Conditions_at_Site,  
    Accidents_2015.Carriageway_Hazards, 
    Accidents_2015.Urban_or_Rural_Area, 
    Accidents_2015.Did_Police_Officer_Attend_Scene_of_Accident, 
    Accidents_2015.LSOA_of_Accident_Location, 
    Vehicles_2015.Accident_Index AS Accident_Index_Vehicles,
    Vehicles_2015.Vehicle_Reference,
    Vehicles_2015.Vehicle_Type,
    Vehicles_2015.Towing_and_Articulation,
    Vehicles_2015.Vehicle_Manoeuvre,
    Vehicles_2015.Vehicle_Location_Restricted_Lane,
    Vehicles_2015.Junction_Location,
    Vehicles_2015.Skidding_and_Overturning,
    Vehicles_2015.Hit_Object_in_Carriageway,
    Vehicles_2015.Vehicle_Leaving_Carriageway,
    Vehicles_2015.Hit_Object_off_Carriageway,
    Vehicles_2015.First_Point_of_Impact,
    Vehicles_2015.Was_Vehicle_Left_Hand_Drive,
    Vehicles_2015.Journey_Purpose_of_Driver,
    Vehicles_2015.Sex_of_Driver,
    Vehicles_2015.Age_of_Driver,
    Vehicles_2015.Age_Band_of_Driver,
    Vehicles_2015.Engine_Capacity_CC,
    Vehicles_2015.Propulsion_Code,
    Vehicles_2015.Age_of_Vehicle,
    Vehicles_2015.Driver_IMD_Decile,
    Vehicles_2015.Driver_Home_Area_Type,
    Vehicles_2015.Vehicle_IMD_Decile    
INTO Combined_Data_Road_Safety_Analysis
FROM Accidents_2015
JOIN Vehicles_2015 ON Accidents_2015.Accident_Index = Vehicles_2015.Accident_Index;

-- 4.1 Vehicle type Analysis
SELECT 
   CASE Vehicle_Type
        WHEN 1 THEN 'Pedal cycle'
        WHEN 2 THEN 'Motorcycle 50cc and under'
        WHEN 3 THEN 'Motorcycle 125cc and under'
        WHEN 4 THEN 'Motorcycle over 125cc and up to 500cc'
        WHEN 5 THEN 'Motorcycle over 500cc'
        WHEN 8 THEN 'Taxi/Private hire car'
        WHEN 9 THEN 'Car'
        WHEN 10 THEN 'Minibus (8 - 16 passenger seats)'
        WHEN 11 THEN 'Bus or coach (17 or more pass seats)'
        WHEN 16 THEN 'Ridden horse'
        WHEN 17 THEN 'Agricultural vehicle'
        WHEN 18 THEN 'Tram'
        WHEN 19 THEN 'Van / Goods 3.5 tonnes mgw or under'
        WHEN 20 THEN 'Goods over 3.5t. and under 7.5t'
        WHEN 21 THEN 'Goods 7.5 tonnes mgw and over'
        WHEN 22 THEN 'Mobility scooter'
        WHEN 23 THEN 'Electric motorcycle'
        WHEN 90 THEN 'Other vehicle'
        WHEN 97 THEN 'Motorcycle - unknown cc'
        WHEN 98 THEN 'Goods vehicle - unknown weight'
        ELSE 'Unknown'
    END AS Vehicle_Type,
    CASE Accident_Severity
        WHEN 1 THEN 'Fatal'
        WHEN 2 THEN 'Serious'  
        WHEN 3 THEN 'Slight'
        ELSE 'Unknown'
    END AS Accident_Severity,
    COUNT(*) AS Accident_Count,
    ROUND(CAST(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (PARTITION BY Vehicle_Type) AS NUMERIC), 2) AS Percentage
FROM 
    Combined_Data_Road_Safety_Analysis
GROUP BY 
    Vehicle_Type,
    Accident_Severity;

-- 4.2 Rank up Vehicle type fatal severity
WITH Fatal_Rank AS (
    SELECT 
        Vehicle_Type,
        ROW_NUMBER() OVER (ORDER BY SUM(CASE WHEN Accident_Severity = 'Fatal' THEN Accident_Count ELSE 0 END) DESC) AS Fatal_Rank
    FROM 
        (
            SELECT 
                CASE Vehicle_Type
                    WHEN 1 THEN 'Pedal cycle'
                    WHEN 2 THEN 'Motorcycle 50cc and under'
                    WHEN 3 THEN 'Motorcycle 125cc and under'
                    WHEN 4 THEN 'Motorcycle over 125cc and up to 500cc'
                    WHEN 5 THEN 'Motorcycle over 500cc'
                    WHEN 8 THEN 'Taxi/Private hire car'
                    WHEN 9 THEN 'Car'
                    WHEN 10 THEN 'Minibus (8 - 16 passenger seats)'
                    WHEN 11 THEN 'Bus or coach (17 or more pass seats)'
                    WHEN 16 THEN 'Ridden horse'
                    WHEN 17 THEN 'Agricultural vehicle'
                    WHEN 18 THEN 'Tram'
                    WHEN 19 THEN 'Van / Goods 3.5 tonnes mgw or under'
                    WHEN 20 THEN 'Goods over 3.5t. and under 7.5t'
                    WHEN 21 THEN 'Goods 7.5 tonnes mgw and over'
                    WHEN 22 THEN 'Mobility scooter'
                    WHEN 23 THEN 'Electric motorcycle'
                    WHEN 90 THEN 'Other vehicle'
                    WHEN 97 THEN 'Motorcycle - unknown cc'
                    WHEN 98 THEN 'Goods vehicle - unknown weight'
                    ELSE 'Unknown'
                END AS Vehicle_Type,
                CASE Accident_Severity
                    WHEN 1 THEN 'Fatal'
                    WHEN 2 THEN 'Serious'  
                    WHEN 3 THEN 'Slight'
                    ELSE 'Unknown'
                END AS Accident_Severity,
                COUNT(*) AS Accident_Count
            FROM 
                Combined_Data_Road_Safety_Analysis
            GROUP BY 
                Vehicle_Type,
                Accident_Severity
        ) AS subquery
    GROUP BY 
        Vehicle_Type
)
SELECT 
    Vehicle_Type,
    Fatal_Rank
FROM 
    Fatal_Rank;

-- 4.3 Demographic Analysis
SELECT
    CASE Sex_of_Driver
            WHEN 1 THEN 'Male'
            WHEN 2 THEN 'Female'
            WHEN 3 THEN 'Not known'
            ELSE 'Data missing or out of range'
    END AS Sex_of_Driver,
    CASE Age_Band_of_Driver
        WHEN 1 THEN '0 - 5'
        WHEN 2 THEN '6 - 10'
        WHEN 3 THEN '11 - 15'
        WHEN 4 THEN '16 - 20'
        WHEN 5 THEN '21 - 25'
        WHEN 6 THEN '26 - 35'
        WHEN 7 THEN '36 - 45'
        WHEN 8 THEN '46 - 55'
        WHEN 9 THEN '56 - 65'
        WHEN 10 THEN '66 - 75'
        WHEN 11 THEN 'Over 75'
        ELSE 'Data missing or out of range'
    END AS Age_Band_of_Driver,
    COUNT(*) AS Accident_Count, ROUND(CAST(COUNT(*)*100.0/SUM(COUNT(*))OVER (PARTITION BY Sex_of_Driver) AS numeric),2) AS PERCENTAGE
    FROM
    Combined_Data_Road_Safety_Analysis
    GROUP BY
        Sex_of_Driver,
        Age_Band_of_Driver;

-- 4.4 Ranked Sex Accident Analysis

WITH DriverSexCounts AS (
    SELECT 
        CASE Sex_of_Driver
            WHEN 1 THEN 'Male'
            WHEN 2 THEN 'Female'
            WHEN 3 THEN 'Not known'
            ELSE 'Data missing or out of range'
        END AS Sex_of_Driver,
        COUNT(*) AS Accident_Count
    FROM
        Combined_Data_Road_Safety_Analysis
    GROUP BY
        Sex_of_Driver
),
RankedDriverSex AS (
    SELECT 
        Sex_of_Driver,
        Accident_Count,
        ROW_NUMBER() OVER (ORDER BY Accident_Count DESC) AS Sex_of_Driver_Rank
    FROM 
        DriverSexCounts
)
SELECT 
    Sex_of_Driver,
    Accident_Count,
    Sex_of_Driver_Rank
FROM 
    RankedDriverSex;

-- 4.4 Ranked Age Band Accident Analysis

WITH AgeBandCounts AS (
    SELECT 
        CASE Age_Band_of_Driver
        WHEN 1 THEN '0 - 5'
        WHEN 2 THEN '6 - 10'
        WHEN 3 THEN '11 - 15'
        WHEN 4 THEN '16 - 20'
        WHEN 5 THEN '21 - 25'
        WHEN 6 THEN '26 - 35'
        WHEN 7 THEN '36 - 45'
        WHEN 8 THEN '46 - 55'
        WHEN 9 THEN '56 - 65'
        WHEN 10 THEN '66 - 75'
        WHEN 11 THEN 'Over 75'
        ELSE 'Data missing or out of range'
    END AS Age_Band_of_Driver,
        COUNT(*) AS Accident_Count
    FROM
        Combined_Data_Road_Safety_Analysis
    GROUP BY
        Age_Band_of_Driver
),
RankedAgeBand AS (
    SELECT 
        Age_Band_of_Driver,
        Accident_Count,
        ROW_NUMBER() OVER (ORDER BY Accident_Count DESC) AS Age_Band_of_Driver_Rank
    FROM 
        AgeBandCounts
)
SELECT 
    Age_Band_of_Driver,
    Accident_Count,
    Age_Band_of_Driver_Rank
FROM 
    RankedAgeBand;

-- 4.5 Vehicle manuever analysis
SELECT 
    CASE Vehicle_Manoeuvre
        WHEN 1 THEN 'Reversing'
        WHEN 2 THEN 'Parked'
        WHEN 3 THEN 'Waiting to go - held up'
        WHEN 4 THEN 'Slowing or stopping'
        WHEN 5 THEN 'Moving off'
        WHEN 6 THEN 'U-turn'
        WHEN 7 THEN 'Turning left'
        WHEN 8 THEN 'Waiting to turn left'
        WHEN 9 THEN 'Turning right'
        WHEN 10 THEN 'Waiting to turn right'
        WHEN 11 THEN 'Changing lane to left'
        WHEN 12 THEN 'Changing lane to right'
        WHEN 13 THEN 'Overtaking moving vehicle - offside'
        WHEN 14 THEN 'Overtaking static vehicle - offside'
        WHEN 15 THEN 'Overtaking - nearside'
        WHEN 16 THEN 'Going ahead left-hand bend'
        WHEN 17 THEN 'Going ahead right-hand bend'
        WHEN 18 THEN 'Going ahead other'
        WHEN 19 THEN 'Turning left - hand bend'
        WHEN 20 THEN 'Turning right - hand bend'
        WHEN 21 THEN 'Going ahead left-hand bend'
        WHEN 22 THEN 'Going ahead right-hand bend'
        WHEN 23 THEN 'Going ahead other'
        WHEN 24 THEN 'Reversing left'
        WHEN 25 THEN 'Reversing right'
        WHEN 26 THEN 'Park left'
        WHEN 27 THEN 'Park right'
        ELSE 'Other'
    END AS Vehicle_Manoeuvre,
    CASE Accident_Severity
        WHEN 1 THEN 'Fatal'
        WHEN 2 THEN 'Serious'  
        WHEN 3 THEN 'Slight'
        ELSE 'Unknown'
    END AS Accident_Severity,
    COUNT(*) AS Accident_Count,
    ROUND(CAST(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (PARTITION BY Vehicle_Manoeuvre) AS NUMERIC), 2) AS Percentage
FROM 
    Combined_Data_Road_Safety_Analysis
GROUP BY 
    Vehicle_Manoeuvre,
    Accident_Severity
ORDER BY 
    Vehicle_Manoeuvre,
    Accident_Severity;

-- 4.6 Rank up Vehicle manuoeuver type fatal severity
WITH Fatal_Rank AS (
    SELECT 
        Vehicle_Manoeuvre,
        ROW_NUMBER() OVER (ORDER BY SUM(CASE WHEN Accident_Severity = 'Fatal' THEN Accident_Count ELSE 0 END) DESC) AS Fatal_Rank
    FROM 
        (
            SELECT 
                CASE Vehicle_Manoeuvre
                    WHEN 1 THEN 'Reversing'
                    WHEN 2 THEN 'Parked'
                    WHEN 3 THEN 'Waiting to go - held up'
                    WHEN 4 THEN 'Slowing or stopping'
                    WHEN 5 THEN 'Moving off'
                    WHEN 6 THEN 'U-turn'
                    WHEN 7 THEN 'Turning left'
                    WHEN 8 THEN 'Waiting to turn left'
                    WHEN 9 THEN 'Turning right'
                    WHEN 10 THEN 'Waiting to turn right'
                    WHEN 11 THEN 'Changing lane to left'
                    WHEN 12 THEN 'Changing lane to right'
                    WHEN 13 THEN 'Overtaking moving vehicle - offside'
                    WHEN 14 THEN 'Overtaking static vehicle - offside'
                    WHEN 15 THEN 'Overtaking - nearside'
                    WHEN 16 THEN 'Going ahead left-hand bend'
                    WHEN 17 THEN 'Going ahead right-hand bend'
                    WHEN 18 THEN 'Going ahead other'
                    WHEN 19 THEN 'Turning left - hand bend'
                    WHEN 20 THEN 'Turning right - hand bend'
                    WHEN 21 THEN 'Going ahead left-hand bend'
                    WHEN 22 THEN 'Going ahead right-hand bend'
                    WHEN 23 THEN 'Going ahead other'
                    WHEN 24 THEN 'Reversing left'
                    WHEN 25 THEN 'Reversing right'
                    WHEN 26 THEN 'Park left'
                    WHEN 27 THEN 'Park right'
                    ELSE 'Other'
                END AS Vehicle_Manoeuvre,
                CASE Accident_Severity
                    WHEN 1 THEN 'Fatal'
                    WHEN 2 THEN 'Serious'  
                    WHEN 3 THEN 'Slight'
                    ELSE 'Unknown'
                END AS Accident_Severity,
                COUNT(*) AS Accident_Count
            FROM 
                Combined_Data_Road_Safety_Analysis
            GROUP BY 
                Vehicle_Manoeuvre,
                Accident_Severity
        ) AS subquery
    GROUP BY 
        Vehicle_Manoeuvre
)
SELECT 
    Vehicle_Manoeuvre,
    Fatal_Rank
FROM 
    Fatal_Rank;

-- 4.7 Journey purpose analysis
SELECT 
    CASE Journey_Purpose_of_Driver
        WHEN 1 THEN 'Journey as part of work'
        WHEN 2 THEN 'Commuting to/from work'
        WHEN 3 THEN 'Taking pupil to/from school'
        WHEN 4 THEN 'Pupil riding to/from school'
        WHEN 5 THEN 'Other'
        WHEN 6 THEN 'Not known'
        WHEN 15 THEN 'Other/Not known (2005-10)'
        ELSE 'Data missing or out of range'
    END AS Journey_Purpose_of_Driver,
    CASE Accident_Severity
        WHEN 1 THEN 'Fatal'
        WHEN 2 THEN 'Serious'  
        WHEN 3 THEN 'Slight'
        ELSE 'Unknown'
    END AS Accident_Severity,
    COUNT(*) AS Accident_Count,
    ROUND(CAST(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (PARTITION BY Journey_Purpose_of_Driver) AS NUMERIC), 2) AS Percentage
FROM 
    Combined_Data_Road_Safety_Analysis
GROUP BY 
    Journey_Purpose_of_Driver,
    Accident_Severity
ORDER BY 
    Journey_Purpose_of_Driver,
    Accident_Severity;

--4.8 Vehicle age and Condition Analysis 
SELECT 
    CASE 
        WHEN Age_of_Vehicle >= 0 AND Age_of_Vehicle <= 5 THEN '0-5 years'
        WHEN Age_of_Vehicle > 5 AND Age_of_Vehicle <= 10 THEN '6-10 years'
        WHEN Age_of_Vehicle > 10 AND Age_of_Vehicle <= 15 THEN '11-15 years'
        WHEN Age_of_Vehicle > 15 AND Age_of_Vehicle <= 20 THEN '16-20 years'
        ELSE 'Over 20 years'
    END AS Age_of_Vehicle,
    Engine_Capacity_CC,
    CASE Accident_Severity
        WHEN 1 THEN 'Fatal'
        WHEN 2 THEN 'Serious'  
        WHEN 3 THEN 'Slight'
        ELSE 'Unknown'
    END AS Accident_Severity,
    COUNT(*) AS Accident_Count,
    ROUND(CAST(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (PARTITION BY Age_of_Vehicle, Engine_Capacity_CC) AS NUMERIC), 2) AS Percentage
FROM 
    Combined_Data_Road_Safety_Analysis
GROUP BY 
    Age_of_Vehicle,
    Engine_Capacity_CC,
    Accident_Severity
ORDER BY 
    Age_of_Vehicle,
    Engine_Capacity_CC,
    Accident_Severity;


--4.8 Vehicle age and Condition type fatal severity
WITH Fatal_Accidents AS (
    SELECT 
        CASE 
            WHEN Age_of_Vehicle >= 0 AND Age_of_Vehicle <= 5 THEN '0-5 years'
            WHEN Age_of_Vehicle > 5 AND Age_of_Vehicle <= 10 THEN '6-10 years'
            WHEN Age_of_Vehicle > 10 AND Age_of_Vehicle <= 15 THEN '11-15 years'
            WHEN Age_of_Vehicle > 15 AND Age_of_Vehicle <= 20 THEN '16-20 years'
            ELSE 'Over 20 years'
        END AS Age_of_Vehicle,
        Engine_Capacity_CC,
        COUNT(*) AS Fatal_Accident_Count
    FROM 
        Combined_Data_Road_Safety_Analysis
    WHERE 
        Accident_Severity = 1
    GROUP BY 
        Age_of_Vehicle,
        Engine_Capacity_CC
)

SELECT 
    Age_of_Vehicle,
    Engine_Capacity_CC,
    Fatal_Accident_Count,
    RANK() OVER (ORDER BY Fatal_Accident_Count DESC) AS Fatal_Rank
FROM 
    Fatal_Accidents
ORDER BY 
    Fatal_Rank;



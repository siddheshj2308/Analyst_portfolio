-- ACCIDENTS_2015 Dataset Analysis 
-- 3.1 Add new columns for Month and Year to the existing table
ALTER TABLE Accidents_2015
ADD [Month] INT, -- Assuming [Month] will store the month number
    [Year] INT;  -- Assuming [Year] will store the year number

-- Update the new columns with month and year values
UPDATE Accidents_2015
SET [Month] = MONTH(CONVERT(DATE, [Date], 103)),
    [Year] = YEAR(CONVERT(DATE, [Date], 103));

-- 3.1 Accident Severity 
SELECT 
    CASE Accident_Severity
        WHEN 1 THEN 'Fatal'
        WHEN 2 THEN 'Serious'  
        WHEN 3 THEN 'Slight'
        ELSE 'Unknown'
    END AS Accident_Severity,
    COUNT(*) AS Accident_Count
FROM
    Accidents_2015
GROUP BY
    Accident_Severity;


--- 3.2 Day_of_week Analysis 
SELECT 
     CASE Day_of_Week
        WHEN 1 THEN 'Sunday'
        WHEN 2 THEN 'Monday'
        WHEN 3 THEN 'Tuesday'
        WHEN 4 THEN 'Wednesday'
        WHEN 5 THEN 'Thursday'
        WHEN 6 THEN 'Friday'
        WHEN 7 THEN 'Saturday'
        ELSE 'Unknown'
    END AS Day_of_Week,
    COUNT(*) AS Accident_Count
FROM
    Accidents_2015
GROUP BY
    Day_of_Week;

-- Rank up Day_of Week Analysis 
WITH DayOfWeekCounts AS (
    SELECT 
        CASE Day_of_Week
            WHEN 1 THEN 'Sunday'
            WHEN 2 THEN 'Monday'
            WHEN 3 THEN 'Tuesday'
            WHEN 4 THEN 'Wednesday'
            WHEN 5 THEN 'Thursday'
            WHEN 6 THEN 'Friday'
            WHEN 7 THEN 'Saturday'
            ELSE 'Unknown'
        END AS Day_of_Week,
        COUNT(*) AS Accident_Count
    FROM
        Accidents_2015
    GROUP BY
        Day_of_Week
),
RankedDays AS (
    SELECT 
        Day_of_Week,
        Accident_Count,
        ROW_NUMBER() OVER (ORDER BY Accident_Count DESC) AS Day_Rank
    FROM 
        DayOfWeekCounts
)
SELECT 
    Day_of_Week,
    Accident_Count,
    Day_Rank
FROM 
    RankedDays;


-- 3.3 Monthly Analysis 
SELECT 
    [Month],
    COUNT(*) AS Accident_Count
FROM
    Accidents_2015
GROUP BY
    [Month];

-- Ranked Monthly Analysis

WITH MonthCounts AS (
    SELECT 
        [Month],
        COUNT(*) AS Accident_Count
    FROM
        Accidents_2015
    GROUP BY
        [Month]
),
RankedMonths AS (
    SELECT 
        [Month],
        Accident_Count,
        ROW_NUMBER() OVER (ORDER BY Accident_Count DESC) AS Month_Rank
    FROM 
        MonthCounts
)
SELECT 
    [Month],
    Accident_Count,
    Month_Rank
FROM 
    RankedMonths;

-- 3.4 Yearly Analysis 
SELECT 
    [Year],
    COUNT(*) AS Accident_Count
FROM
    Accidents_2015
GROUP BY
    [Year];

-- 3.5 Seasonal Analysis 
SELECT 
    CASE [Month]
        WHEN 12 THEN 'Winter'
        WHEN 1 THEN 'Winter'
        WHEN 2 THEN 'Winter'
        WHEN 3 THEN 'Spring'
        WHEN 4 THEN 'Spring'
        WHEN 5 THEN 'Spring'
        WHEN 6 THEN 'Summer'
        WHEN 7 THEN 'Summer'
        WHEN 8 THEN 'Summer'
        WHEN 9 THEN 'Autumn'
        WHEN 10 THEN 'Autumn'
        WHEN 11 THEN 'Autumn'
        ELSE 'Unknown'
    END AS [Month],
    COUNT(*) AS Accident_Count
FROM
    Accidents_2015
GROUP BY
    [Month];

-- 3.6 Weather Impact Analysis
SELECT 
   CASE Weather_Conditions
        WHEN 1 THEN 'Fine no high winds'
        WHEN 2 THEN 'Raining no high winds'
        WHEN 3 THEN 'Snowing no high winds'
        WHEN 4 THEN 'Fine + high winds'
        WHEN 5 THEN 'Raining + high winds'
        WHEN 6 THEN 'Snowing + high winds'
        WHEN 7 THEN 'Fog or mist'
        WHEN 8 THEN 'Other'
        WHEN 9 THEN 'Unknown'
        ELSE 'Data missing or out of range'
    END AS Weather_Conditions,
    CASE Accident_Severity
        WHEN 1 THEN 'Fatal'
        WHEN 2 THEN 'Serious'  
        WHEN 3 THEN 'Slight'
        ELSE 'Unknown'
    END AS Accident_Severity,
    COUNT(*) AS Accident_Count,
    ROUND(CAST(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (PARTITION BY Weather_Conditions) AS NUMERIC), 2) AS Percentage
FROM 
    Accidents_2015
GROUP BY 
    Weather_Conditions,
    Accident_Severity;

-- 3.7 Road Serface Condition Analysis

SELECT 
   CASE Road_Surface_Conditions
        WHEN 1 THEN 'Dry'
        WHEN 2 THEN 'Wet or damp'
        WHEN 3 THEN 'Snow'
        WHEN 4 THEN 'Frost or ice'
        WHEN 5 THEN 'Flood over 3cm. deep'
        WHEN 6 THEN 'Oil or diesel'
        WHEN 7 THEN 'Mud'
        ELSE 'Data missing or out of range'
    END AS Road_Surface_Conditions, 
    CASE Accident_Severity
        WHEN 1 THEN 'Fatal'
        WHEN 2 THEN 'Serious'  
        WHEN 3 THEN 'Slight'
        ELSE 'Unknown'
    END AS Accident_Severity,
    COUNT(*) AS Accident_Count,
    ROUND(CAST(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (PARTITION BY Road_Surface_Conditions) AS NUMERIC), 2) AS Percentage
FROM 
    Accidents_2015
GROUP BY 
    Road_Surface_Conditions,
    Accident_Severity;

-- 3.8 Urban or Rural Accident Analysis
SELECT 
   CASE Urban_or_Rural_Area
        WHEN 1 THEN 'Urban'
        WHEN 2 THEN 'Rural'
        ELSE 'Unallocated'
    END AS Urban_or_Rural_Area, 
    CASE Accident_Severity
        WHEN 1 THEN 'Fatal'
        WHEN 2 THEN 'Serious'  
        WHEN 3 THEN 'Slight'
        ELSE 'Unknown'
    END AS Accident_Severity,
    COUNT(*) AS Accident_Count,
    ROUND(CAST(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (PARTITION BY Urban_or_Rural_Area) AS NUMERIC), 2) AS Percentage
FROM 
    Accidents_2015
GROUP BY 
    Urban_or_Rural_Area,
    Accident_Severity;

-- 3.9 Light Condition Analysis
SELECT 
   CASE Light_Conditions
        WHEN 1 THEN 'Daylight'
        WHEN 4 THEN 'Darkness - lights lit'
        WHEN 5 THEN 'Darkness - lights unlit'
        WHEN 6 THEN 'Darkness - no lighting'
        WHEN 7 THEN 'Darkness - lighting unknown'
        ELSE 'Data missing or out of range'
    END AS Light_Conditions,  
    CASE Accident_Severity
        WHEN 1 THEN 'Fatal'
        WHEN 2 THEN 'Serious'  
        WHEN 3 THEN 'Slight'
        ELSE 'Unknown'
    END AS Accident_Severity,
    COUNT(*) AS Accident_Count,
    ROUND(CAST(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (PARTITION BY Light_Conditions) AS NUMERIC), 2) AS Percentage
FROM 
    Accidents_2015
GROUP BY 
    Light_Conditions,
    Accident_Severity;

-- 3.10 Accidents severity by AREA

SELECT 
   CASE Police_Force
        WHEN 1 THEN	'Metropolitan Police'
        WHEN 3 THEN	'Cumbria'
        WHEN 4 THEN	'Lancashire'
        WHEN 5 THEN	'Merseyside'
        WHEN 6 THEN	'Greater Manchester'
        WHEN 7 THEN	'Cheshire'
        WHEN 10 THEN 'Northumbria'
        WHEN 11 THEN 'Durham'
        WHEN 12 THEN 'North Yorkshire'
        WHEN 13 THEN 'West Yorkshire'
        WHEN 14 THEN 'South Yorkshire'
        WHEN 16 THEN 'Humberside'
        WHEN 17 THEN 'Cleveland'
        WHEN 20	THEN 'West Midlands'
        WHEN 21 THEN 'Staffordshire'
        WHEN 22 THEN 'West Mercia'
        WHEN 23 THEN 'Warwickshire'
        WHEN 30 THEN 'Derbyshire'
        WHEN 31 THEN 'Nottinghamshire'
        WHEN 32 THEN 'Lincolnshire'
        WHEN 33 THEN 'Leicestershire'
        WHEN 34 THEN 'Northamptonshire'
        WHEN 35 THEN 'Cambridgeshire'
        WHEN 36 THEN 'Norfolk'
        WHEN 37 THEN 'Suffolk'
        WHEN 40 THEN 'Bedfordshire'
        WHEN 41 THEN 'Hertfordshire'
        WHEN 42 THEN 'Essex'
        WHEN 43	THEN 'Thames Valley'
        WHEN 44	THEN 'Hampshire'
        WHEN 45 THEN 'Surrey'
        WHEN 46 THEN 'Kent'
        WHEN 47 THEN 'Sussex'
        WHEN 48 THEN 'City of London'
        WHEN 50 THEN 'Devon and Cornwall'
        WHEN 52	THEN 'Avon and Somerset'
        WHEN 53	THEN 'Gloucestershire'
        WHEN 54	THEN 'Wiltshire'
        WHEN 55	THEN 'Dorset'
        WHEN 60	THEN 'North Wales'
        WHEN 61	THEN 'Gwent'
        WHEN 62	THEN 'South Wales'
        WHEN 63	THEN 'Dyfed-Powys'
        WHEN 91	THEN 'Northern'
        WHEN 92	THEN 'Grampian'
        WHEN 93	THEN 'Tayside'
        WHEN 94	THEN 'Fife'
        WHEN 95	THEN 'Lothian and Borders'
        WHEN 96	THEN 'Central'
        WHEN 97	THEN 'Strathclyde'
        WHEN 98	THEN 'Dumfries and Galloway'
        ELSE 'Unknown'
    END AS Police_Force,   
    CASE Accident_Severity
        WHEN 1 THEN 'Fatal'
        WHEN 2 THEN 'Serious'  
        WHEN 3 THEN 'Slight'
        ELSE 'Unknown'
    END AS Accident_Severity,
    COUNT(*) AS Accident_Count,
    ROUND(CAST(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (PARTITION BY Police_Force) AS NUMERIC), 2) AS Percentage
FROM 
    Accidents_2015
GROUP BY 
    Police_Force,
    Accident_Severity;

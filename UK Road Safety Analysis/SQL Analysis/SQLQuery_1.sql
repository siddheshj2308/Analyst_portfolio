--- Road safety Analysis

-- 1. Load Data into Dataset 

USE tempdb; -- Specify the database

-- Create a staging table
CREATE TABLE dbo.Accidents_2015_Staging (
    Accident_Index VARCHAR(MAX),
    Location_Easting_OSGR VARCHAR(MAX),
    Location_Northing_OSGR VARCHAR(MAX),
    Longitude VARCHAR(MAX),
    Latitude VARCHAR(MAX),
    Police_Force VARCHAR(MAX),
    Accident_Severity VARCHAR(MAX),
    Number_of_Vehicles VARCHAR(MAX),
    Number_of_Casualties VARCHAR(MAX),
    [Date] VARCHAR(MAX),
    Day_of_Week VARCHAR(MAX),
    [Time] VARCHAR(MAX),
    Local_Authority_District VARCHAR(MAX),
    Local_Authority_Highway VARCHAR(MAX),
    First_Road_Class VARCHAR(MAX),
    First_Road_Number VARCHAR(MAX),
    Road_Type VARCHAR(MAX),
    Speed_limit VARCHAR(MAX),
    Junction_Detail VARCHAR(MAX),
    Junction_Control VARCHAR(MAX),
    Second_Road_Class VARCHAR(MAX),
    Second_Road_Number VARCHAR(MAX),
    Pedestrian_Crossing_Human_Control VARCHAR(MAX),
    Pedestrian_Crossing_Physical_Facilities VARCHAR(MAX),
    Light_Conditions VARCHAR(MAX),
    Weather_Conditions VARCHAR(MAX),
    Road_Surface_Conditions VARCHAR(MAX),
    Special_Conditions_at_Site VARCHAR(MAX),
    Carriageway_Hazards VARCHAR(MAX),
    Urban_or_Rural_Area VARCHAR(MAX),
    Did_Police_Officer_Attend_Scene_of_Accident VARCHAR(MAX),
    LSOA_of_Accident_Location VARCHAR(MAX)
);
-- Insert data from CSV file into the staging table
BULK INSERT dbo.Accidents_2015_Staging
FROM '/Accidents_2015.csv'
WITH (
    FIELDTERMINATOR = ',',
    ROWTERMINATOR = '\n',
    FIRSTROW = 2 -- Skip header row if present
);

USE tempdb; -- or specify your desired database

-- Drop existing table if it exists
IF OBJECT_ID('dbo.Accidents_2015', 'U') IS NOT NULL
BEGIN
    DROP TABLE dbo.Accidents_2015;
END

-- Create a new table to store the CSV data
CREATE TABLE dbo.Accidents_2015 (
    Accident_Index VARCHAR(20),
    Location_Easting_OSGR INT,
    Location_Northing_OSGR INT,
    Longitude FLOAT,
    Latitude FLOAT,
    Police_Force INT,
    Accident_Severity INT,
    Number_of_Vehicles INT,
    Number_of_Casualties INT,
    [Date] VARCHAR(10),
    Day_of_Week INT,
    [Time] VARCHAR(8), -- Assuming the time format is HH:MM:SS
    Local_Authority_District VARCHAR(20),
    Local_Authority_Highway VARCHAR(20),
    First_Road_Class INT,
    First_Road_Number INT,
    Road_Type INT,
    Speed_limit INT,
    Junction_Detail INT,
    Junction_Control INT,
    Second_Road_Class INT,
    Second_Road_Number INT,
    Pedestrian_Crossing_Human_Control INT,
    Pedestrian_Crossing_Physical_Facilities INT,
    Light_Conditions INT,
    Weather_Conditions INT,
    Road_Surface_Conditions INT,
    Special_Conditions_at_Site INT,
    Carriageway_Hazards INT,
    Urban_or_Rural_Area INT,
    Did_Police_Officer_Attend_Scene_of_Accident INT,
    LSOA_of_Accident_Location VARCHAR(20)
);

-- Insert data into the final table with proper data types
INSERT INTO dbo.Accidents_2015 (Accident_Index, Location_Easting_OSGR, Location_Northing_OSGR, Longitude, Latitude, Police_Force, Accident_Severity, Number_of_Vehicles, Number_of_Casualties, [Date], Day_of_Week, [Time], Local_Authority_District, Local_Authority_Highway, First_Road_Class, First_Road_Number, Road_Type, Speed_limit, Junction_Detail, Junction_Control, Second_Road_Class, Second_Road_Number, Pedestrian_Crossing_Human_Control, Pedestrian_Crossing_Physical_Facilities, Light_Conditions, Weather_Conditions, Road_Surface_Conditions, Special_Conditions_at_Site, Carriageway_Hazards, Urban_or_Rural_Area, Did_Police_Officer_Attend_Scene_of_Accident, LSOA_of_Accident_Location)
SELECT 
    Accident_Index, 
    Location_Easting_OSGR, 
    Location_Northing_OSGR, 
    Longitude, 
    Latitude, 
    Police_Force, 
    Accident_Severity, 
    Number_of_Vehicles, 
    Number_of_Casualties, 
    [Date],
    TRY_CAST(Day_of_Week AS INT), -- Convert Day_of_Week to INT data type
    TRY_CAST([Time] AS TIME), -- Convert Time to TIME data type
    Local_Authority_District, 
    Local_Authority_Highway, 
    First_Road_Class, 
    First_Road_Number, 
    Road_Type, 
    Speed_limit, 
    Junction_Detail, 
    Junction_Control, 
    Second_Road_Class, 
    Second_Road_Number, 
    Pedestrian_Crossing_Human_Control, 
    Pedestrian_Crossing_Physical_Facilities, 
    Light_Conditions, 
    Weather_Conditions, 
    Road_Surface_Conditions, 
    Special_Conditions_at_Site, 
    Carriageway_Hazards, 
    Urban_or_Rural_Area, 
    Did_Police_Officer_Attend_Scene_of_Accident, 
    LSOA_of_Accident_Location
FROM dbo.Accidents_2015_Staging;


USE tempdb; -- or specify your desired database

-- Create a staging table
CREATE TABLE dbo.Vehicles_2015_Staging (
    Accident_Index VARCHAR(MAX),
    Vehicle_Reference INT,
    Vehicle_Type INT,
    Towing_and_Articulation INT,
    Vehicle_Manoeuvre INT,
    Vehicle_Location_Restricted_Lane INT,
    Junction_Location INT,
    Skidding_and_Overturning INT,
    Hit_Object_in_Carriageway INT,
    Vehicle_Leaving_Carriageway INT,
    Hit_Object_off_Carriageway INT,
    First_Point_of_Impact INT,
    Was_Vehicle_Left_Hand_Drive INT,
    Journey_Purpose_of_Driver INT,
    Sex_of_Driver INT,
    Age_of_Driver INT,
    Age_Band_of_Driver INT,
    Engine_Capacity_CC INT,
    Propulsion_Code INT,
    Age_of_Vehicle INT,
    Driver_IMD_Decile INT,
    Driver_Home_Area_Type INT,
    Vehicle_IMD_Decile INT
);

-- Insert data from CSV file into the staging table
BULK INSERT dbo.Vehicles_2015_Staging
FROM '/Vehicles_2015.csv'
WITH (
    FIELDTERMINATOR = ',',
    ROWTERMINATOR = '\n',
    FIRSTROW = 2 -- Skip header row if present
);

-- Drop existing table if it exists
IF OBJECT_ID('dbo.Vehicles_2015', 'U') IS NOT NULL
BEGIN
    DROP TABLE dbo.Vehicles_2015;
END

-- Create a new table to store the data
CREATE TABLE dbo.Vehicles_2015 (
    Accident_Index VARCHAR(20),
    Vehicle_Reference INT,
    Vehicle_Type INT,
    Towing_and_Articulation INT,
    Vehicle_Manoeuvre INT,
    Vehicle_Location_Restricted_Lane INT,
    Junction_Location INT,
    Skidding_and_Overturning INT,
    Hit_Object_in_Carriageway INT,
    Vehicle_Leaving_Carriageway INT,
    Hit_Object_off_Carriageway INT,
    First_Point_of_Impact INT,
    Was_Vehicle_Left_Hand_Drive INT,
    Journey_Purpose_of_Driver INT,
    Sex_of_Driver INT,
    Age_of_Driver INT,
    Age_Band_of_Driver INT,
    Engine_Capacity_CC INT,
    Propulsion_Code INT,
    Age_of_Vehicle INT,
    Driver_IMD_Decile INT,
    Driver_Home_Area_Type INT,
    Vehicle_IMD_Decile INT
);

-- Insert data from the staging table into the final table
INSERT INTO dbo.Vehicles_2015 (Accident_Index, Vehicle_Reference, Vehicle_Type, Towing_and_Articulation, Vehicle_Manoeuvre, Vehicle_Location_Restricted_Lane, Junction_Location, Skidding_and_Overturning, Hit_Object_in_Carriageway, Vehicle_Leaving_Carriageway, Hit_Object_off_Carriageway, First_Point_of_Impact, Was_Vehicle_Left_Hand_Drive, Journey_Purpose_of_Driver, Sex_of_Driver, Age_of_Driver, Age_Band_of_Driver, Engine_Capacity_CC, Propulsion_Code, Age_of_Vehicle, Driver_IMD_Decile, Driver_Home_Area_Type, Vehicle_IMD_Decile)
SELECT 
    Accident_Index,
    Vehicle_Reference,
    Vehicle_Type,
    Towing_and_Articulation,
    Vehicle_Manoeuvre,
    Vehicle_Location_Restricted_Lane,
    Junction_Location,
    Skidding_and_Overturning,
    Hit_Object_in_Carriageway,
    Vehicle_Leaving_Carriageway,
    Hit_Object_off_Carriageway,
    First_Point_of_Impact,
    Was_Vehicle_Left_Hand_Drive,
    Journey_Purpose_of_Driver,
    Sex_of_Driver,
    Age_of_Driver,
    Age_Band_of_Driver,
    Engine_Capacity_CC,
    Propulsion_Code,
    Age_of_Vehicle,
    Driver_IMD_Decile,
    Driver_Home_Area_Type,
    Vehicle_IMD_Decile
FROM dbo.Vehicles_2015_Staging;

-- 2. Renaming numerical Observations with Given Description 

SELECT 
    Accident_Index, 
    Location_Easting_OSGR, 
    Location_Northing_OSGR, 
    Longitude, 
    Latitude, 
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
    Number_of_Vehicles, 
    Number_of_Casualties, 
    [Date],
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
    [Time],
    Local_Authority_District, 
    Local_Authority_Highway, 
    CASE First_Road_Class
        WHEN 1 THEN 'Motorway'
        WHEN 2 THEN 'A(M)'
        WHEN 3 THEN 'A'
        WHEN 4 THEN 'B'
        WHEN 5 THEN 'C'
        WHEN 6 THEN 'Unclassified' 
        ELSE 'unknown'
    END AS First_Road_Class, 
    First_Road_Number, 
    CASE Road_Type
        WHEN 1 THEN 'Roundabout'
        WHEN 2 THEN 'One way street'
        WHEN 3 THEN 'Dual carriageway'
        WHEN 6 THEN 'Single carriageway'
        WHEN 7 THEN 'Slip road'
        WHEN 9 THEN 'Unknown'
        WHEN 12 THEN'One way street/Slip road'
        ELSE 'Data missing or out of range'
    END AS Road_Type, 
    Speed_limit, 
    CASE Junction_Detail
        WHEN 0 THEN 'Not at junction or within 20 metres'
        WHEN 1 THEN 'Roundabout'
        WHEN 2 THEN 'Mini-roundabout'
        WHEN 3 THEN 'T or staggered junction'
        WHEN 5 THEN 'Slip road'
        WHEN 6 THEN 'Crossroads'
        WHEN 7 THEN 'More than 4 arms (not roundabout)'
        WHEN 8 THEN 'Private drive or entrance'
        WHEN 9 THEN 'Other junction'
    ELSE 'Data missing or out of range'
    END AS Junction_Detail, 
    Junction_Control, 
    CASE Second_Road_Class
        WHEN 0 THEN 'Not at junction or within 20 metres'
        WHEN 1 THEN 'Motorway'
        WHEN 2 THEN 'A(M)' 
        WHEN 3 THEN 'A'
        WHEN 4 THEN 'B'
        WHEN 5 THEN'C'
    ELSE 'Unclassified'
    END AS Second_Road_Class, 
    Second_Road_Number, 
    CASE Pedestrian_Crossing_Human_Control
        WHEN 0 THEN 'None within 50 metres '
        WHEN 1 THEN 'Control by school crossing patrol'
        WHEN 2 THEN 'Control by other authorised person'
        ELSE 'Data missing or out of range'
    END AS Pedestrian_Crossing_Human_Control, 
    CASE Pedestrian_Crossing_Physical_Facilities
        WHEN 0 THEN 'No physical crossing facilities within 50 metres'
        WHEN 1 THEN 'Zebra'
        WHEN 4 THEN 'Pelican, puffin, toucan or similar non-junction pedestrian light crossing'
        WHEN 5 THEN 'Pedestrian phase at traffic signal junction'
        WHEN 7 THEN 'Footbridge or subway'
        WHEN 8 THEN 'Central refuge'
    ELSE 'Data missing or out of range'
    END AS Pedestrian_Crossing_Physical_Facilities, 
    CASE Light_Conditions
        WHEN 1 THEN 'Daylight'
        WHEN 4 THEN 'Darkness - lights lit'
        WHEN 5 THEN 'Darkness - lights unlit'
        WHEN 6 THEN 'Darkness - no lighting'
        WHEN 7 THEN 'Darkness - lighting unknown'
        ELSE 'Data missing or out of range'
    END AS Light_Conditions, 
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
    CASE Special_Conditions_at_Site
        WHEN 0 THEN 'None'
        WHEN 1 THEN 'Auto traffic signal - out'
        WHEN 2 THEN 'Auto signal part defective'
        WHEN 3 THEN 'Road sign or marking defective or obscured'
        WHEN 4 THEN 'Roadworks'
        WHEN 5 THEN 'Road surface defective'
        WHEN 6 THEN 'Oil or diesel'
        WHEN 7 THEN 'Mud'
        ELSE 'Data missing or out of range'
    END AS Special_Conditions_at_Site, 
    CASE Carriageway_Hazards
        WHEN 0 THEN 'None'
        WHEN 1 THEN 'Vehicle load on road'
        WHEN 2 THEN 'Other object on road'
        WHEN 3 THEN 'Previous accident'
        WHEN 4 THEN 'Dog on road'
        WHEN 5 THEN 'Other animal on road'
        WHEN 6 THEN 'Pedestrian in carriageway - not injured'
        WHEN 7 THEN 'Any animal in carriageway (except ridden horse)'
        ELSE 'Data missing or out of range'
    END AS Carriageway_Hazards,  
    CASE Urban_or_Rural_Area
        WHEN 1 THEN 'Urban'
        WHEN 2 THEN 'Rural'
        ELSE 'Unallocated'
    END AS Urban_or_Rural_Area,  
    CASE Did_Police_Officer_Attend_Scene_of_Accident
        WHEN 1 THEN 'Yes'
        WHEN 2 THEN 'No'
        ELSE 'No - accident was reported using a self completion  form (self rep only)'
    END AS Did_Police_Officer_Attend_Scene_of_Accident, 
    LSOA_of_Accident_Location
FROM dbo.Accidents_2015;


SELECT 
    Accident_Index,
    Vehicle_Reference,
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
    CASE Towing_and_Articulation
        WHEN 0 THEN 'No tow/articulation'
        WHEN 1 THEN 'Articulated vehicle'
        WHEN 2 THEN 'Double or multiple trailer'
        WHEN 3 THEN 'Caravan'
        WHEN 4 THEN 'Single trailer'
        WHEN 5 THEN 'Other tow'
        ELSE 'Data missing or out of range'
    END AS Towing_and_Articulation,
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
        ELSE 'Data missing or out of range'
    END AS Vehicle_Manoeuvre,
    CASE Vehicle_Location_Restricted_Lane
        WHEN 0 THEN 'On main cway - not in restricted lane'
        WHEN 1 THEN 'Tram/Light rail track'
        WHEN 2 THEN 'Bus lane'
        WHEN 3 THEN 'Busway (including guided busway)'
        WHEN 4 THEN 'Cycle lane (on main carriageway)'
        WHEN 5 THEN 'Cycleway or shared use footway (not part of  main carriageway)'
        WHEN 6 THEN 'On lay-by or hard shoulder'
        WHEN 7 THEN 'Entering lay-by or hard shoulder'
        WHEN 8 THEN 'Leaving lay-by or hard shoulder'
        WHEN 9 THEN 'Footway (pavement)'
        WHEN 10 THEN 'Not on carriageway'
        ELSE 'Data missing or out of range'
    END AS Vehicle_Location_Restricted_Lane,
    CASE Junction_Location
        WHEN 0 THEN 'Not at or within 20 metres of junction'
        WHEN 1 THEN 'Approaching junction or waiting/parked at junction approach'
        WHEN 2 THEN 'Cleared junction or waiting/parked at junction exit'
        WHEN 3 THEN 'Leaving roundabout'
        WHEN 4 THEN 'Entering roundabout'
        WHEN 5 THEN 'Leaving main road'
        WHEN 6 THEN 'Entering main road'
        WHEN 7 THEN 'Entering from slip road'
        WHEN 8 THEN 'Mid Junction - on roundabout or on main road'
        ELSE 'Data missing or out of range'
    END AS Junction_Location,
    CASE Skidding_and_Overturning
        WHEN 0 THEN 'None'
        WHEN 1 THEN 'Skidded'
        WHEN 2 THEN 'Skidded and overturned'
        WHEN 3 THEN 'Jackknifed'
        WHEN 4 THEN 'Jackknifed and overturned'
        WHEN 5 THEN 'Overturned'
        ELSE 'Data missing or out of range'
    END AS Skidding_and_Overturning,
    CASE Hit_Object_in_Carriageway
        WHEN 0 THEN 'None'
        WHEN 1 THEN 'Previous accident'
        WHEN 2 THEN 'Road works'
        WHEN 4 THEN 'Parked vehicle'
        WHEN 5 THEN 'Bridge (roof)'
        WHEN 6 THEN 'Bridge (side)'
        WHEN 7 THEN 'Bollard or refuge'
        WHEN 8 THEN 'Open door of vehicle'
        WHEN 9 THEN 'Central island of roundabout'
        WHEN 10 THEN 'Kerb'
        WHEN 11 THEN 'Other object'
        WHEN 12 THEN 'Any animal (except ridden horse)'
        ELSE 'Data missing or out of range'
    END AS Hit_Object_in_Carriageway,
    CASE Vehicle_Leaving_Carriageway
        WHEN 0 THEN 'Did not leave carriageway'
        WHEN 1 THEN 'Nearside'
        WHEN 2 THEN 'Nearside and rebounded'
        WHEN 3 THEN 'Straight ahead at junction'
        WHEN 4 THEN 'Offside on to central reservation'
        WHEN 5 THEN 'Offside on to centrl res + rebounded'
        WHEN 6 THEN 'Offside - crossed central reservation'
        WHEN 7 THEN 'Offside'
        WHEN 8 THEN 'Offside and rebounded'
        ELSE 'Data missing or out of range'
    END AS Vehicle_Leaving_Carriageway,
    Hit_Object_off_Carriageway,
    CASE First_Point_of_Impact
        WHEN 0 THEN 'Did not impact'
        WHEN 1 THEN 'Front'
        WHEN 2 THEN 'Back'
        WHEN 3 THEN 'Offside'
        WHEN 4 THEN 'Nearside'
        ELSE 'Data missing or out of range'
    END AS First_Point_of_Impact,
    CASE Was_Vehicle_Left_Hand_Drive
        WHEN 1 THEN 'No'
        WHEN 2 THEN 'Yes'
        ELSE 'Data missing or out of range'
    END AS Was_Vehicle_Left_Hand_Drive,
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
    CASE Sex_of_Driver
        WHEN 1 THEN 'Male'
        WHEN 2 THEN 'Female'
        WHEN 3 THEN 'Not known'
        ELSE 'Data missing or out of range'
    END AS Sex_of_Driver,
    Age_of_Driver,
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
    Engine_Capacity_CC,
    CASE Propulsion_Code
        WHEN 1 THEN 'Petrol'
        WHEN 2 THEN 'Heavy oil'
        WHEN 3 THEN 'Electric'
        WHEN 4 THEN 'Steam'
        WHEN 5 THEN 'Gas'
        WHEN 6 THEN 'Petrol/Gas (LPG)'
        WHEN 7 THEN 'Gas/Bi-fuel'
        WHEN 8 THEN 'Hybrid electric'
        WHEN 9 THEN 'Gas Diesel'
        WHEN 10 THEN 'New fuel technology'
        WHEN 11 THEN 'Fuel cells'
        WHEN 12 THEN 'Electric diesel'
        ELSE 'Undefined'
    END AS Propulsion_Code,
    Age_of_Vehicle,
    Driver_IMD_Decile,
    Driver_Home_Area_Type,
    Vehicle_IMD_Decile
FROM dbo.Vehicles_2015;
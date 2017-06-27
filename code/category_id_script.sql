# source E:/BB101/_專題/real_estate/codecategory_id_script.sql;

ALTER TABLE comp6 ADD COLUMN category_id4 char(2) AFTER industry_code4;

UPDATE comp6 SET category_id4 = 

CASE 
    WHEN industry_code4 = "959916" THEN "21" 
    WHEN industry_code4 = "959914" THEN "22" 
    WHEN industry_code4 = "931219" THEN "23" 
    WHEN industry_code4 = "931218" THEN "24" 
    WHEN industry_code4 = "750000" THEN "25" 
    WHEN industry_code4 = "591411" THEN "26" 
    WHEN industry_code4 = "562112" THEN "27" 
    WHEN industry_code4 = "561015" THEN "28" 
    WHEN industry_code4 = "561014" THEN "29" 
    WHEN industry_code4 = "561013" THEN "30" 
    WHEN industry_code4 = "561012" THEN "31" 
    WHEN industry_code4 = "561011" THEN "32" 
    WHEN industry_code4 = "551013" THEN "33" 
    WHEN industry_code4 = "551012" THEN "34" 
    WHEN industry_code4 = "551011" THEN "35" 
    WHEN industry_code4 = "524100" THEN "36" 
    WHEN industry_code4 = "475112" THEN "37" 
    WHEN industry_code4 = "475112" THEN "38" 
    WHEN industry_code4 = "475111" THEN "39" 
    WHEN industry_code4 = "471911" THEN "40" 
    WHEN industry_code4 = "471116" THEN "41" 
    WHEN industry_code4 = "471115" THEN "42" 
    WHEN industry_code4 = "471112" THEN "43" 
    WHEN industry_code4 = "471113" THEN "43" 
    WHEN industry_code4 = "471114" THEN "43" 
    WHEN industry_code4 = "471111" THEN "44" 
	
    ELSE 
    (CASE 
        WHEN industry_code4 like "9621%" THEN "45" 
        WHEN industry_code4 like "9610%" THEN "46" 
        WHEN industry_code4 like "9591%" THEN "47" 
        WHEN industry_code4 like "9522%" THEN "48" 
        WHEN industry_code4 like "9521%" THEN "49" 
        WHEN industry_code4 like "9512%" THEN "50" 
        WHEN industry_code4 like "9511%" THEN "51" 
        WHEN industry_code4 like "9312%" THEN "52" 
        WHEN industry_code4 like "8620%" THEN "53" 
        WHEN industry_code4 like "8610%" THEN "54" 
        WHEN industry_code4 like "6412%" THEN "55" 
        WHEN industry_code4 like "5631%" THEN "56" 
        WHEN industry_code4 like "5622%" THEN "57" 
        WHEN industry_code4 like "5621%" THEN "58" 
        WHEN industry_code4 like "4832%" THEN "59" 
        WHEN industry_code4 like "4831%" THEN "60" 
        WHEN industry_code4 like "4821%" THEN "61" 
        WHEN industry_code4 like "4761%" THEN "62" 
        WHEN industry_code4 between "01" AND "04" THEN "01" 
        WHEN industry_code4 between "05" AND "08" THEN "02" 
        WHEN industry_code4 between "08" AND "35" THEN "03" 
        WHEN industry_code4 between "35" AND "36" THEN "04" 
        WHEN industry_code4 between "36" AND "40" THEN "05" 
        WHEN industry_code4 between "41" AND "44" THEN "06" 
        WHEN industry_code4 between "45" AND "49" THEN "07" 
        WHEN industry_code4 between "49" AND "55" THEN "08" 
        WHEN industry_code4 between "55" AND "57" THEN "09" 
        WHEN industry_code4 between "58" AND "64" THEN "10" 
        WHEN industry_code4 between "64" AND "67" THEN "11" 
        WHEN industry_code4 between "67" AND "69" THEN "12" 
        WHEN industry_code4 between "69" AND "77" THEN "13" 
        WHEN industry_code4 between "77" AND "83" THEN "14" 
        WHEN industry_code4 between "83" AND "85" THEN "15" 
        WHEN industry_code4 between "85" AND "86" THEN "16" 
        WHEN industry_code4 between "86" AND "89" THEN "17" 
        WHEN industry_code4 between "90" AND "94" THEN "18" 
        WHEN industry_code4 between "94" AND "97" THEN "19"
    	END)
	END
	;

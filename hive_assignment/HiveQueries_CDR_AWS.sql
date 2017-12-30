-- Task 1
-- Creating external table cdr & inserting CDR data from S3 bucket.

create external table cdr
(square_id int,time_interval double,country_code int,sms_in_activity float,sms_out_acvtivity float,
call_in_activity float,call_out_activity float,internet_traffic_activity float)
row format
delimited fields terminated by '\t'
lines terminated by '\n'
tblproperties('serialization.null.format' = '');

load data inpath 's3://mohitassignment/CDR.txt' overwrite into table cdr;

select * from cdr; -- Checking table data, empty values are replaced with NULL

desc cdr -- Checking table structure

-- Task 2
-- Creating a table(cdr_subset) from the external table created in task 1(cdr), to store the data corresponding only to these three countries.

create table cdr_subset as select * from cdr where country_code in(39,43,33);

select * from cdr_subset; -- Checking cdr_subset data

-- Q1: How many grids (square_ids) are there in total in the given three countries? Display the number of unique grids in the three countries.

select count(distinct(square_id)) total_grids from cdr_subset; -- 240

-- Q2: Which country has the minimum total internet activity? Display the country code of this country.

select country_code,count(internet_traffic_activity) as total_internet_activity from cdr_subset group by country_code order by total_internet_activity limit 1; -- Its country_code 43, Internet Activity: 33:1823,39:34297,43:9,

-- Q3: Which country among the given three has the second highest total activity? Note that total activity is defined as the sum of sms_in, sms_out, call_in, call_out, internet_traffic. Display the country code of this country. Do not compress the table.

select country_code,sum(sms_in_activity)+sum(sms_out_acvtivity)+sum(call_in_activity)+sum(call_out_activity)+sum(internet_traffic_activity) as total_activity from cdr_subset group by country_code order by total_activity; -- Its country_code 33, Total Avtivity: 43:16.608, 33:205.907, 39:568225.050,

-- Q4: Which squareID has the maximum total SMS activity in these three countries? Note that total SMS activity is the sum of incoming and outgoing SMSes

select square_id,sum(sms_in_activity)+sum(sms_out_acvtivity) as total_SMS_activity from cdr_subset group by square_id order by total_SMS_activity desc limit 1; -- 1151 2063.551

-- Q5: What is the total activity for the three countries? Note that total activity is defined as the sum of sms_in, sms_out, call_in, call_out, internet_traffic. For this task, first compress the table that was created in the beginning of task 2. Submit the result upto three decimal places, without rounding off the digits, obtained after compressing the table.

create table cdr_subset_orc stored as ORC as select * from cdr_subset; -- Creating compressed table in orc format

select sum(sms_in_activity)+sum(sms_out_acvtivity)+sum(call_in_activity)+sum(call_out_activity)+sum(internet_traffic_activity) as total_activity from cdr_subset_orc; -- 568447.56658968516

-- Task 3
-- Creating partition table
create external table cdr_partition3 (square_id int,time_interval double,sms_in_activity float,sms_out_acvtivity float,call_in_activity float,call_out_activity float,internet_traffic_activity float) partitioned by (country_code int) row format delimited fields terminated by '\t' ;

-- Updating hive dynamic partition config to avoid errors
SET hive.exec.dynamic.partition = true;
SET hive.exec.dynamic.partition.mode = nonstrict;
SET hive.exec.max.dynamic.partitions=100000;
SET hive.exec.max.dynamic.partitions.pernode=100000;

--Inserting data in external paartition table
insert overwrite table cdr_partition3 partition(country_code) select square_id,time_interval,sms_in_activity,sms_out_acvtivity,call_in_activity,call_out_activity,internet_traffic_activity,country_code from cdr_subset;

select * from cdr_partition3; -- Checking partition table data

-- Q1: What is the total call activity from the three square_ids to country_code 39? Submit the result upto three decimal places, without rounding off the digits.

select sum(call_in_activity)+sum(call_out_activity) as total_call_activity from cdr_partition3 where country_code=39 and square_id in(101,102,103); -- 290.821

-- Q2: What is the total SMS activity from the three square_ids to country_code 39? Submit the result upto three decimal places, without rounding off the digits.

select sum(sms_in_activity)+sum(sms_out_acvtivity) as total_sms_activity from cdr_partition3 where country_code=39 and square_id in(101,102,103); -- 622.232

-- Q3: What is the total activity, i.e. sum of CallIn, CallOut, SMSIn, SMSOut, internet traffic of the three square_ids? Submit the result upto three decimal places, without rounding off the digits. You may either compress the partitioned table or not.

select sum(sms_in_activity)+sum(sms_out_acvtivity)+sum(call_in_activity)+sum(call_out_activity)+sum(internet_traffic_activity) as total_activity from cdr_partition3 where country_code=39 and square_id in(101,102,103); -- 5344.636

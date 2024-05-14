CREATE TABLE `Table1`(
	`ID` int,
    `Air Pressure (9 am)` VARCHAR(4),
    `Temperature (9 am)` VARCHAR(4),
    `Rain (3 pm)` int
);

INSERT INTO `Table1` VALUES(1,'High','Cold',1);
INSERT INTO `Table1` VALUES(2,'Low','Warm',1);
INSERT INTO `Table1` VALUES(3,'High','Warm',0);
INSERT INTO `Table1` VALUES(4,'High','Warm',0);
INSERT INTO `Table1` VALUES(5,'Low','Warm',1);
INSERT INTO `Table1` VALUES(6,'High','Warm',0);
INSERT INTO `Table1` VALUES(7,'High','Warm',0);
INSERT INTO `Table1` VALUES(8,'High','Warm',0);
INSERT INTO `Table1` VALUES(9,'High','Warm',0);
INSERT INTO `Table1` VALUES(10,'Low','Cold',1);
INSERT INTO `Table1` VALUES(11,'Low','Warm',1);
INSERT INTO `Table1` VALUES(12,'High','Warm',0);
INSERT INTO `Table1` VALUES(13,'High','Warm',0);
INSERT INTO `Table1` VALUES(14,'Low','Cold',1);
INSERT INTO `Table1` VALUES(15,'Low','Warm',1);
INSERT INTO `Table1` VALUES(16,'High','Warm',0);
INSERT INTO `Table1` VALUES(17,'Low','Warm',1);
INSERT INTO `Table1` VALUES(18,'Low','Cold',1);
INSERT INTO `Table1` VALUES(19,'High','Warm',0);
INSERT INTO `Table1` VALUES(20,'High','Cold',1);

SELECT * FROM `Table1`;

SELECT ID FROM Table1 
WHERE `Air Pressure (9 am)` = 'High' AND `Temperature (9 am)` = 'Cold';

CREATE TABLE `Table2`(
	`ID` int,
    `Wind Direction (9 am)` float,
    `Wind Speed (9 am)` float,
    `Rain Yesterday` int,
  	`Rain (3 pm)` int
);
INSERT INTO `Table2` VALUES(1, 73.40, 10.65, 1, 1);
INSERT INTO `Table2` VALUES(2, 179.50, 7.02, 0, 1);
INSERT INTO `Table2` VALUES(5, 166.40, 3.85, 0, 1);
INSERT INTO `Table2` VALUES(10, 211.80, 2.01, 0, 1);
INSERT INTO `Table2` VALUES(11, 100.20, 2.13, 0, 1);
INSERT INTO `Table2` VALUES(14, 187.30, 11.81, 1, 1);
INSERT INTO `Table2` VALUES(15, 184.50, 6.87, 0, 1);
INSERT INTO `Table2` VALUES(17, 179.00, 5.26, 0, 1);
INSERT INTO `Table2` VALUES(18, 150.50, 2.33, 1, 1);
INSERT INTO `Table2` VALUES(20, 151.00, 3.31, 0, 1);
SELECT * FROM `Table2`;

SELECT *
FROM Table1
NATURAL JOIN Table2

SELECT *
FROM Table1
NATURAL JOIN Table2
WHERE `Wind Direction (9 am)` < 185 AND `Wind Speed (9 am)` > 3 AND `Temperature (9 am)` = 'Cold';

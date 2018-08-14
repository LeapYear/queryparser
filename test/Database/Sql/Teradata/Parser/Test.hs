-- Copyright (c) 2017 Uber Technologies, Inc.
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

module Database.Sql.Teradata.Parser.Test where

import Test.HUnit
import Database.Sql.Type
import Database.Sql.Position
import Database.Sql.Teradata.Parser
import Database.Sql.Teradata.Type

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

parsesSuccessfully :: Text -> Assertion
parsesSuccessfully sql = case parse sql of
    (Right _) -> return ()
    (Left e) -> assertFailure $ unlines
        [ "failed to parse:"
        , show sql
        , show e
        ]

parsesUnsuccessfully :: Text -> Assertion
parsesUnsuccessfully sql = case parse sql of
    (Left _) -> return ()
    (Right _) -> assertFailure $ unlines
        [ "parsed broken sql:"
        , show sql
        ]

testParser :: Test
testParser = test
    [ "Parse some arbitrary examples" ~: map (TestCase . parsesSuccessfully)
        [ "SELECT 1;"
        , ";"
        , "SELECT foo FROM bar;"
        , "SELECT id AS foo FROM bar;"
        , "SELECT id foo FROM bar;"
        , "SELECT bar.id foo FROM bar;"
        , "SELECT 1 UNION SELECT 2;"
        , "SELECT * FROM foo INNER JOIN bar ON foo.id = bar.id;"
        , "SELECT * FROM foo JOIN bar ON foo.id = bar.id;"
        , "SELECT * FROM foo NATURAL JOIN bar;"
        , "WITH foo AS (SELECT 1) SELECT * FROM foo NATURAL JOIN bar;"
        , "SELECT f.uuid, /* comment */ count(1) FROM foo f GROUP BY 1;"
        , "SELECT f.x, count(distinct f.x) FROM foo f GROUP BY 1;"
        , "SELECT sum(x = 7) FROM foo f;"
        , "SELECT CASE WHEN x = 7 THEN 1 ELSE 0 END FROM foo f;"
        , "SELECT sum(CASE WHEN x = 7 THEN 1 ELSE 0 END) FROM foo f;"
        , "SELECT CASE 1 WHEN 2 THEN 2 WHEN NULLSEQUAL 3 THEN 5 END;"
        , "SELECT CASE 1 WHEN 2 THEN 2 WHEN NULLSEQUAL 3 THEN 5 ELSE 4 END;"
        , "SELECT 0, 0.1, .01, 1.;"
        , TL.unwords
            [ "SELECT CASE WHEN SUM(foo) = 0 then 0 ELSE"
            , "ROUND(SUM(CASE WHEN (bar = 'Yes')"
            , "THEN baz ELSE 0 END)/(SUM(qux) + .000000001), 2.0) END"
            , "AS \"Stuff. Y'know?\";"
            ]

        , "SELECT foo WHERE bar IS NULL;"
        , "SELECT foo WHERE bar IS NOT NULL;"
        , "SELECT foo WHERE bar IS TRUE;"
        , "SELECT foo WHERE bar IS UNKNOWN;"
        , TL.unlines
            [ "SELECT foo"
            , "FROM baz -- this is a comment"
            , "WHERE bar IS NOT NULL;"
            ]

        , "SELECT 1=1;"
        , "SELECT 1!=3, 1<>4;"
        , "SELECT 1<=>5;"
        , "SELECT 1>1, 1>=1, 1<1, 1<=1;"
        , "SELECT TRUE;"
        , "SELECT NULL;"
        , "SELECT COUNT(*);"
        , TL.unwords
            [ "SELECT site,date,num_hits,an_rank()"
            , "over (partition by site order by num_hits desc);"
            ]

        , TL.unwords
            [ "SELECT site,date,num_hits,an_rank()"
            , "over (partition by site, blah order by num_hits desc);"
            ]

        , "SELECT CAST(1 as int);"
        , "SELECT 1 || 2;"
        , "SELECT current_timestamp AT TIMEZONE 'America/Los_Angeles';"
        , "SELECT (DATE '2007-02-16', DATE '2007-12-21') OVERLAPS (DATE '2007-10-30', DATE '2008-10-30');"
        , "SELECT 1 IN (1, 2);"
        , "SELECT 1 NOT IN (1, 2);"
        , "SELECT LEFT('foo', 7);"
        , "SELECT ((1));"
        , "((SELECT 1));"
        , TL.unwords
            [ "SELECT name FROM \"user\""
            , "WHERE \"user\".id IN (SELECT user_id FROM whatever);"
            ]
        , "SELECT 1 WHERE EXISTS (SELECT 1);"
        , "SELECT 1 WHERE NOT EXISTS (SELECT 1);"
        , "SELECT 1 WHERE NOT NOT EXISTS (SELECT 1);"
        , "SELECT 1 WHERE NOT NOT NOT EXISTS (SELECT 1);"
        , "SELECT CURRENT_TIME;"
        , "SELECT SYSDATE;"
        , "SELECT SYSDATE();"
        , "SELECT (3 // 2) as integer_division;"
        , "SELECT 'foo' LIKE 'bar' ESCAPE '!';"
        , "SELECT 1 AS ESCAPE;"
        , "SELECT deptno, sal, empno, COUNT(*) OVER (PARTITION BY deptno ORDER BY sal ROWS BETWEEN 2 PRECEDING AND CURRENT ROW);"
        , "INSERT INTO foo DEFAULT VALUES;"
        , "INSERT INTO foo VALUES (1,2,2+3);"
        , "INSERT INTO foo (a, b, c) SELECT * FROM baz WHERE qux > 3;"
        , "INSERT INTO foo (SELECT * FROM baz);"
        , TL.unlines
          [ "MERGE INTO foo.bar t1 USING foo.baz t2 ON t1.a = t2.a"
          , "WHEN MATCHED THEN UPDATE SET b = t2.b, c = t2.c"
          , "WHEN NOT MATCHED THEN INSERT (b, c) VALUES (t2.b, t2.c)"
          ]
        , TL.unlines
          [ "MERGE INTO foo.bar t1 USING foo.baz t2 ON t1.a = t2.a"
          , "WHEN MATCHED THEN UPDATE SET b = t2.b, c = t2.c"
          ]
        , TL.unlines
          [ "MERGE INTO foo.bar t1 USING foo.baz t2 ON t1.a = t2.a"
          , "WHEN NOT MATCHED THEN INSERT (b, c) VALUES (t2.b, t2.c)"
          ]
        , "SELECT 2 = ALL(ARRAY[1,2]);"

        , "SELECT * FROM foo AS a;"
        , "SELECT * FROM (foo JOIN bar ON baz);"
        , "SELECT 1 FROM ((foo JOIN bar ON blah) JOIN baz ON qux);"
        , "SELECT 1 FROM ((foo JOIN bar ON blah) JOIN baz ON qux) AS wat;"
        , "SELECT 1 AS (a, b);"
        , "SELECT * FROM ((SELECT 1)) as a;"
        , TL.unlines
            [ "with a as (select 1),"
            , "     b as (select 2),"
            , "     c as (select 3)"
            , "select * from ((a join b on true) as foo join c on true);"
            ]
        , "SELECT ISNULL(1, 2);"
        , "SELECT 1 NOTNULL;"
        , "SELECT 1 ISNULL;"
        , "SELECT COUNT(1 USING PARAMETERS wat=7);"
        , "SELECT (interval '1' month), interval '1 month', interval '2' day as bar;"
        , "SELECT slice_time, symbol, TS_LAST_VALUE(bid IGNORE NULLS) AS last_bid FROM TickStore;"
        , "SELECT +1, -2;"
        , "SELECT zone FROM foo;"
        , "SELECT foo as order FROM bar;"
        , "DELETE FROM foo;"
        , "DELETE FROM foo WHERE EXISTS (SELECT * FROM bar WHERE foo.a = bar.b);"
        , "DROP TABLE foo;"
        , "DROP TABLE IF EXISTS foo CASCADE;"
        , "BEGIN;"
        , "BEGIN TRANSACTION ISOLATION LEVEL READ UNCOMMITTED READ WRITE;"
        , "BEGIN TRANSACTION ISOLATION LEVEL READ COMMITTED READ WRITE;"
        , "BEGIN WORK ISOLATION LEVEL REPEATABLE READ READ ONLY;"
        , "BEGIN WORK ISOLATION LEVEL SERIALIZABLE READ ONLY;"
        , "START TRANSACTION ISOLATION LEVEL SERIALIZABLE READ ONLY;"
        , "COMMIT;"
        , "COMMIT WORK;"
        , "COMMIT TRANSACTION;"
        , "END;"
        , "END WORK;"
        , "END TRANSACTION;"
        , "ROLLBACK;"
        , "ROLLBACK WORK;"
        , "ROLLBACK TRANSACTION;"
        , "ABORT;"
        , "ABORT WORK;"
        , "ABORT TRANSACTION;"
        , "EXPLAIN SELECT 1;"
        , "EXPLAIN DELETE FROM foo;"
        , "EXPLAIN INSERT INTO foo SELECT 1;"
        , "SELECT 1^2;"
        , "SELECT 1" -- the `;` is optional
        , "SELECT 1\n  " -- the `;` is optional and trailing whitespace is ok
        , "WITH x AS (SELECT 1 AS a FROM dual)  SELECT * FROM X             UNION  SELECT * FROM X ORDER BY a ;"
        , "WITH x AS (SELECT 1 AS a FROM dual) (SELECT * FROM X ORDER BY a) UNION (SELECT * FROM X ORDER BY a);"
        , "WITH x AS (SELECT 1 AS a FROM dual) (SELECT * FROM X ORDER BY a) UNION (SELECT * FROM X) ORDER BY a;"
        , "WITH x AS (SELECT 1 AS a FROM dual) (SELECT * FROM X ORDER BY a) UNION  SELECT * FROM X ORDER BY a ;"
        , "WITH x AS (SELECT 1 AS a FROM dual) ((SELECT * FROM X ORDER BY a) UNION SELECT * FROM X) ORDER BY a;"

        , "SELECT 1 AS OR;"
        , "SELECT 1 AS AND;"
        , "SELECT t.'a' FROM (SELECT 1 AS 'a') t;"
        ]

    , "Parse examples from Teradata documentation:" ~: map (TestCase . parsesSuccessfully)
        [ "SELECT deptno, name, salary FROM personnel.employee WHERE deptno IN(100, 500) ORDER BY deptno, name;"
        , "SELECT ADD_MONTHS (CURRENT_DATE, 12*13);"
        , "SELECT ADD_MONTHS (CURRENT_DATE, -6);"
        , "SELECT DATE, TIME;"
        , "SELECT x1,x2 FROM t1,t2 WHERE t1.x1=t2.x2;"
        , "SELECT x1 FROM t1 WHERE x1 IN (SELECT x2 FROM t2);"
        , "SELECT name FROM personnel p WHERE salary = (SELECT MAX(salary) FROM personnel sp WHERE p.department=sp.department);"
        , "SELECT pubname FROM publisher WHERE 0 = (SELECT COUNT(*) FROM book WHERE book.pubnum=publisher.pubnum);"
        , "SELECT (fix_cost + (SELECT SUM(part_cost) FROM parts)) AS total_cost FROM fixes;"
        , "SELECT (SELECT prod_name FROM prod_table AS p WHERE p.pno = s.pno) || store_no from stores AS s;"
        , "SELECT CASE WHEN (SELECT count(*) FROM inventory WHERE inventory.pno = orders.pno) > 0 THEN 1 ELSE 0 END FROM orders;"
        , "SELECT SUM(SELECT count(*) FROM sales WHERE sales.txn_no = receipts.txn_no) FROM receipts;"
        , "SELECT * FROM receipts WHERE txn_no IN (1,2, (SELECT MAX(txn_no) FROM sales WHERE sale_date = CURRENT_DATE));"
        , "SELECT category, title,(SELECT AVG(price) FROM movie_titles AS t1 WHERE t1.category=t2.category) AS avgprice FROM movie_titles AS t2 WHERE t2.price < avgprice;"
        , "WITH orderable_items (product_id, quantity) AS (SELECT stocked.product_id, stocked.quantity FROM stocked, product WHERE stocked.product_id = product.product_id AND product.on_hand > 5) SELECT product_id, quantity FROM orderable_items WHERE quantity < 10;"
        , "SELECT product_id, quantity FROM (SELECT stocked.product_id, stocked.quantity FROM stocked, product WHERE stocked.product_id = product.product_id AND product.on_hand > 5) AS orderable_items WHERE quantity < 10;"
        , "WITH multiple_orders AS (SELECT customer_id, COUNT(*) AS order_count FROM orders GROUP BY customer_id HAVING COUNT(*) > 1), multiple_order_totals AS (SELECT customer_id, SUM(total_cost) AS total_spend FROM orders WHERE customer_id IN (SELECT customer_id FROM multiple_orders) GROUP BY customer_id) SELECT * FROM multiple_order_totals ORDER BY total_spend DESC;"
        , "WITH multiple_order_totals AS (SELECT customer_id, SUM(total_cost) AS total_spend FROM orders WHERE customer_id IN (SELECT customer_id FROM multiple_orders) GROUP BY customer_id), multiple_orders AS (SELECT customer_id, COUNT(*) AS order_count FROM orders GROUP BY customer_id HAVING COUNT(*) > 1) SELECT * FROM multiple_order_totals ORDER BY total_spend DESC;"
        , "SELECT DISTINCT dept_no FROM employee;"
        , "SELECT ALL dept_no FROM employee;"
        , "SELECT workers.name, workers.yrs_exp, workers.dept_no, managers.name, managers.yrs_exp FROM employee AS workers, employee AS managers WHERE managers.dept_no = workers.dept_no AND UPPER (managers.jobtitle) IN ('MANAGER' OR 'VICE PRES') AND workers.yrs_exp > managers.yrs_exp; "
        , "SELECT skills.skill_name, emp.emp_no FROM skills LEFT OUTER JOIN emp ON skills.skill_no=emp.skill_no;"
        , "SELECT * FROM (SELECT t1.col1, t1.col2, t1.col3, t2.col1, t2.col2, t2.col3 FROM tab1 AS t1, tab2 AS t2 WHERE t1.col2=t2.col3) AS derived_table (t1_col1, t1_col2, t1_col3, t2_col1, t2_col2, t2_col3);"
        , "SELECT name, salary, average_salary FROM (SELECT AVG(salary) FROM employee) AS workers (average_salary), employee WHERE salary > average_salary ORDER BY salary DESC;"
        , "SELECT name, salary, dept_no, average_salary FROM (SELECT AVG(salary), dept_no FROM employee GROUP BY dept_no) AS workers (average_salary,dept_num), employee WHERE salary > average_salary AND dept_num = dept_no ORDER BY dept_no, salary DESC;"
        , "SELECT * FROM employee WHERE dept = 100 AND (edlev >= 16 OR yrsexp >= 5);"
        , "SELECT name, dept_no FROM employee WHERE UPPER (job_title) LIKE '%ANALYST%';"
        , "SELECT name, jobtitle FROM employee WHERE dept_no = 100;"
        , "SELECT employee.* FROM employee, department WHERE employee.empno=department.mgr_no; "
        , "SELECT category, title, price FROM movie_titles AS t2 WHERE (SELECT AVG(price) FROM movie_titles AS t1 WHERE t1.category = t2.category)<(SELECT AVG(price) FROM movie_titles);"
        , "SELECT name, loc FROM employee, department WHERE employee.dept_no = department.dept_no AND employee.dept_no IN (SELECT dept_no FROM department WHERE mgr_no = 10007);"
        , "SELECT name, dept_no, jobtitle, salary FROM employee WHERE salary > (SELECT AVG(salary) FROM employee) ORDER BY name;"
        , "SELECT * FROM table_1 WHERE x < ALL (SELECT y FROM table_2 WHERE table_2.n = table_1.n);"
        , "SELECT * FROM table_1 WHERE x = (SELECT y FROM table_2 WHERE table_2.n = table_1.n AND t2.m = (SELECT y FROM table_3 WHERE table_3.n = table_2.m AND table_1.l = table_3.l));"
        , "SELECT * FROM table_1 AS a WHERE x < (SELECT AVG(table_1.x) FROM table_1 WHERE table_1.n = a.n);"
        , "SELECT * FROM table_1 WHERE x < (SELECT AVG(a.x) FROM table_1 AS a WHERE table_1.n = a.n);"
        , "SELECT * FROM employee AS e1 WHERE age < (SELECT MAX(age) FROM employee AS e2 WHERE e1.sex = e2.sex);"
        , "SELECT 101, 'Friedrich', 'F', 23 FROM employee AS e1 WHERE 23 < (SELECT MAX(age) FROM employee AS e2 WHERE 'F' = e2.sex);"
        , "SELECT column_list_1 FROM table_list_1 WHERE predicate_1 (SELECT column_list_2 FROM table_list_2 WHERE predicate_2);"
        , "SELECT * FROM table_1 WHERE col_1 IN (SELECT MAX(col_3) FROM table_2 WHERE table_1.col_2=table_2.col_4);"
        , "SELECT name FROM student st1 WHERE age < ALL (SELECT age FROM student st2 WHERE st1.grade = st2.grade AND st1.stno <> st2.stno);"
        , "SELECT pub_name, book_count FROM library WHERE book_count IN (SELECT count(*) FROM book WHERE book.pub_num = library.pub_num);"
        , "SELECT y, m, r, s, SUM(u) FROM test GROUP BY ROLLUP(y,m),ROLLUP(r,s) ORDER BY 1,2,3,4;"
        , "SELECT (SELECT MAX(b3) FROM t3) AS ssq2, (SELECT (SEL ssq2) + a1 FROM t1) AS ssq1 FROM t2;"
        , "SELECT y,m,r, SUM(u) FROM test GROUP BY CUBE(y,m), CUBE(r) ORDER BY 1,2,3;"
        , "SELECT y,m,r,SUM(u) FROM test GROUP BY CUBE(y,m,r) ORDER BY 1,2,3;"
        , "SELECT department_number, SUM(salary_amount) FROM employee WHERE department_number IN (100, 200, 300);"
        , "SELECT department_number, SUM(salary_amount) FROM employee WHERE department_number IN (100, 200, 300) GROUP BY department_number;"
        , "SELECT employee.dept_no, department.dept_name, AVG(salary) FROM employee, department WHERE employee.dept_no = department.dept_no GROUP BY employee.dept_no;"
        , "SELECT min(a1) as i, max(b1) as j from t1 GROUP BY c1, d1 HAVING 30 >= (sel count(*) from t2 where t1.d1=5);"
        , "SELECT sale_date, SUM(amount) FROM sales_table AS s GROUP BY sale_date, (SELECT prod_name FROM prod_table AS p WHERE p.prod_no = s.prod_no);"
        , "SELECT emp_no, period_of_stay FROM employee GROUP BY emp_no, period_of_stay;"
        , "SELECT pid, county, SUM(sale) FROM sales_view GROUP BY CUBE (pid,county);"
        , "SELECT COUNT(employee) FROM department WHERE dept_no = 100 HAVING COUNT(employee) > 10;"
        , "SELECT SUM(t.a) FROM t,u HAVING SUM(t.a)=SUM(u.a);"
        , "SELECT SUM(t.a), SUM(u.a) FROM t,u HAVING SUM(t.b)=SUM(u.b);"
        , "SELECT SUM(t.a) FROM t,u HAVING SUM(t.b)=SUM(u.b) AND u.b = 1 GROUP BY u.b;"
        , "SELECT dept_no, MIN(salary), MAX(salary), AVG(salary) FROM employee WHERE dept_no IN (100,300,500,600) GROUP BY dept_no HAVING AVG(salary) > 37000;"
        , "SELECT table_1.category, (table_2.price - table_2.cost) * SUM (table_1.sales_qty) AS margin FROM sales_hist AS table_1, unit_price_cost AS table_2 WHERE table_1.prod_no=table_2.prodno GROUP BY table_1.category, table_2.price, table_2.cost HAVING margin > 1000;"
        , "SELECT name, jobtitle, yrs_exp FROM employee ORDER BY yrsexp;"
        , "SELECT name, dept_no FROM employee ORDER BY dept_no, salary;"
        , "SELECT name, jobtitle, yrs_expe, jobtitle, yrs FROM employee ORDER BY 3;"
        , "SELECT t1.i, SUM(t1.j) AS j, t1.k FROM t1 GROUP BY 1,3 ORDER BY j;"
        , "SELECT t1.i, SUM(t1.j) AS jj, t1.k FROM t1 GROUP BY 1,3 ORDER BY jj;"
        , "SELECT COUNT(name), dept_no FROM employee GROUP BY dept_no ORDER BY COUNT(name);"
        , "SELECT name, dept_no, salary FROM employee ORDER BY 2 ASC, 3 DESC; SELECT name, dept_no, salary FROM employee ORDER BY dept_no, salary DESC;"
        , "SELECT Loc FROM department, employee WHERE employee.name = 'Marston A' AND employee.deptno = department.deptno;"
        , "SELECT company_name, sales FROM cust_prod_sales AS a, cust_file AS b WHERE a.cust_no = b.cust_no AND a.pcode = 123 AND a.sales > 10000;"
        , "SELECT * FROM employee AS e JOIN project_details AS pd ON e.period_of_stay = pd.project_period;"
        , "SELECT workers.name, workers.yrs_exp, workers.dept_no, managers.name, managers.yrsexp FROM employee AS workers, employee AS managers WHERE managers.dept_no = workers.dept_no AND managers.job_title IN ('Manager', 'Vice Pres') AND workers.yrs_exp > managers.yrs_exp;"
        , "SELECT offerings.course_no, offerings.location, enrollment.emp_no FROM offerings LEFT OUTER JOIN enrollment ON offerings.course_no = employee.course_no;"
        , "SELECT x1, y1, x2, y2 FROM t1, t2 WHERE x1=x2;"
        , "SELECT x1, y1, x2, y2 FROM t1 LEFT OUTER JOIN t2 ON x1=x2;"
        , "SELECT column_expression FROM table_a LEFT OUTER JOIN table_b ON join_conditions WHERE join_condition_exclusions;"
        , "SELECT category, title, COUNT(*) FROM movie_titles AS t2 LEFT OUTER JOIN transactions AS txn ON (SELECT AVG(price) FROM movie_titles AS t1 WHERE t1.category = t2.category)<(SELECT AVG(price) FROM movie_titles) AND t2.title = txn.title;"
        , "SELECT offerings.course_no,offerings.location,enrollment.emp_no FROM offerings LEFT OUTER JOIN enrollment ON offerings.course_no = enrollment.courseno;"
        , "SELECT offerings.course_no,offerings.location,enrollment.emp_no FROM offerings LEFT JOIN enrollment ON offerings.course_no = employee.course_no;"
        , "SELECT offerings.course_no, offerings.location, enrollment.emp_no FROM offerings RIGHT OUTER JOIN enrollment ON offerings.course_no = enrollment.course_no;"
        , "SELECT enrollment.course_no,offerings.location,enrollment.emp_no FROM offerings RIGHT OUTER JOIN enrollment ON offerings.course_no = enrollment.course_no;"
        , "SELECT courses.course_no, offerings.course_no, offerings.location, enrollment.course_no, enrollment.emp_no FROM offerings FULL OUTER JOIN enrollment ON offerings.course_no = enrollment.course_no RIGHT OUTER JOIN courses ON courses.course_no = offerings.course_no;"
        , "SELECT courses.course_no, offerings.course_no, enrollment.course_no, enrollment.emp_no FROM offerings FULL OUTER JOIN enrollment ON offerings.course_no = enrollment.course_no RIGHT OUTER JOIN courses ON courses.course_no = enrollment.course_no;"
        , "SELECT offerings.course_no, enrollment.emp_no FROM offerings LEFT JOIN enrollment ON offerings.course_no = enrollment.course_no WHERE location = 'El Segundo';"
        , "SELECT offerings.course_no, enrollment.emp_no FROM offerings LEFT OUTER JOIN enrollment ON (location = 'El Segundo') AND (offerings.course_no = enrollment.course_no);"
        , "SELECT * FROM table_a LEFT OUTER JOIN table_b ON table_a.a = table_b.a WHERE table_a.b > 5;"
        , "SELECT * FROM table_a LEFT OUTER JOIN table_b ON (table_a.b > 5) AND (table_a.a = table_b.a);"
        , "SELECT offerings.course_no, enrollment.emp_no FROM offerings LEFT OUTER JOIN enrollment ON offerings.course_no = enrollment.course_no AND enrollment.emp_no = 236;"
        , "SELECT c.cust_num FROM sampdb.customer c WHERE c.district = 'K' AND (c.service_type = 'ABC' OR c.service_type = 'XYZ') ORDER BY 1;"
        , "SELECT c.custnum, b.monthly_revenue FROM sampdb.customer AS c, sampdb2.revenue AS b WHERE c.custnum = b.custnum AND c.district = 'K' AND b.data_year_month = 199707 AND (c.service_type = 'ABC' OR c.service_type = 'XYZ') ORDER BY 1;"
        , "SELECT c.custnum, b.monthly_revenue FROM sampdb.customer AS c LEFT OUTER JOIN sampdb2.revenue AS b ON c.custnum = b.custnum WHERE c.district ='K' AND b.data_date = 199707 AND (c.service_type = 'ABC' OR c.service_type = 'XYZ') ORDER BY 1;"
        , "SELECT c.custnum, b.monthly_revenue FROM sampdb.customer AS c LEFT OUTER JOIN sampdb2.revenue AS b ON c.custnum = b.custnum AND c.district ='K' AND b.data_year_month = 199707 AND (c.service_type ='ABC' OR c.service_type ='XYZ') ORDER BY 1;"
        , "SELECT c.custnum, b.monthly_revenue FROM sampdb.customer c LEFT OUTER JOIN sampdb2.revenue b ON c.custnum = b.custnum AND c.district ='K' AND (c.service_type = 'ABC' OR c.service_type = 'XYZ') WHERE b.data_year_month = 199707 ORDER BY 1;"
        , "SELECT c.custnum, b.monthly_revenue FROM sampdb.customer AS c LEFT OUTER JOIN sampdb2.revenue AS b ON c.custnum = b.custnum AND b.data_year_month = 199707 WHERE c.district ='K' AND (c.service_type = 'ABC' OR c.service_type = 'XYZ') ORDER BY 1;"
        , "SELECT deptno, name, salary FROM personnel.employee WHERE deptno IN(100, 500) ORDER BY deptno, name;"
        , "SELECT Salary FROM Employee WHERE EmpNo = 10005;"
        , "SELECT CHARACTER_LENGTH(Details) FROM Orders;"
        , "SELECT AVG(Salary) FROM Employee;"
        ]

    , "Failing examples from Teradata documentation:" ~: map (TestCase . parsesUnsuccessfully)
        [ "SELECT adname, cart_amt FROM attribute_sales ( ON (SELECT cookie, cart_amt FROM weblog WHERE page = 'thankyou' ) as W PARTITION BY cookie ON adlog as S PARTITION BY cookie USING clicks(.8) impressions(.2)) AS D1(adname,attr_revenue);"
        , "SELECT pid, sid FROM closest_store (ON phone_purchases PARTITION BY pid, ON stores DIMENSION) AS D;"
        , "WITH RECURSIVE temp_table (employee_number) AS (SELECT root.employee_number FROM employee AS root WHERE root.manager_employee_number = 801 UNION ALL SELECT indirect.employee_number FROM temp_table AS direct, employee AS indirect WHERE direct.employee_number = indirect.manager_employee_number) SELECT * FROM temp_table ORDER BY employee_number;"
        , "WITH RECURSIVE orderable_items (product_id, quantity) AS (SELECT stocked.product_id, stocked.quantity FROM stocked, product WHERE stocked.product_id = product.product_id AND product.on_hand > 5) SELECT product_id, quantity FROM orderable_items WHERE quantity < 10;"
        , "WITH RECURSIVE temp_table (employee_id, level) AS (SELECT root.employee_number, 0 AS level FROM employee AS root WHERE root.employee_number = 1003 UNION ALL SELECT direct.employee_id, direct.level+1 FROM temp_table AS direct, employee AS indir WHERE indir.employee_number IN (1003,1004) AND direct.level < 2) SELECT * FROM temp_table ORDER BY level;"
        , "SELECT TOP 10 * FROM sales ORDER BY county;"
        , "SELECT * FROM sales QUALIFY ROW_NUMBER() OVER (ORDER BY COUNTY) <= 10; "
        , "SELECT TOP 10 WITH TIES * FROM sales ORDER BY county; "
        , "SELECT * FROM sales QUALIFY RANK() OVER (ORDER BY county) <= 10;"
        , "SELECT item, COUNT(store) FROM (SELECT store,item,profit,QUANTILE(100,profit) AS percentile FROM (SELECT ds.store, it.item, SUM(sales)- COUNT(sales) * it.item_cost AS profit FROM daily_sales AS ds, items AS it WHERE ds.item = it.item GROUP BY ds.store, it.item, it.item_cost) AS item_profit GROUP BY store, item, profit QUALIFY percentile = 0) AS top_one_percent GROUP BY item HAVING COUNT(store) >= 20;"
        , "SELECT * FROM (SELECT item, profit, RANK(profit) AS profit_rank FROM item, sales QUALIFY profit_rank <= 100 AS p) FULL OUTER JOIN (SELECT item, revenue, RANK(revenue) AS revenue_rank FROM item, sales QUALIFY revenue_rank <= 100 AS r) ON p.item = r.item;"
        , "SELECT store, item, profit, QUANTILE(100, profit) AS percentile FROM (SELECT items.item, SUM(sales) - (COUNT(sales)*items.item_cost) AS profit FROM daily_sales, items WHERE daily_sales.item = items.item GROUP BY items.item,items.itemcost) AS item_profit GROUP BY store, item, profit, percentile QUALIFY percentile = 99;"
        , "SELECT itemid, sumprice, RANK() OVER (ORDER BY sumprice DESC) FROM (SELECT a1.item_id, SUM(a1.sale) FROM sales AS a1 GROUP BY a1.itemID) AS t1 (item_id, sumprice) QUALIFY RANK() OVER (ORDER BY sum_price DESC) <=100;"
        , "SELECT item, profit, QUANTILE(100, profit) AS percentile FROM (SELECT item, SUM(sales)-(COUNT(sales)*items.itemcost) AS profit FROM daily_sales, items WHERE daily_sales.item = items.item GROUP BY item) AS itemprofit QUALIFY percentile = 99;"
        , "SELECT line, da, mon, inc, NULLIFZERO(COUNT(inc) OVER(PARTITION BY mon ORDER BY mon, line ROWS BETWEEN 1 FOLLOWING AND UNBOUNDED FOLLOWING) FROM demogr WHERE yr = 94 AND mon < 13 AND da < 10 QUALIFY NULLIFZERO(COUNT(inc)OVER(PARTITION BY mon ORDER BY mon, line ROWS BETWEEN 1 FOLLOWING AND UNBOUNDED FOLLOWING) < 3;"
        , "SELECT y,m,r,SUM(u) FROM test GROUP BY GROUPING SETS(y,()), GROUPING SETS(m,()), GROUPING SETS(r,()) ORDER BY 1,2,3;"
        , "SELECT y, m, r, s, SUM(u) FROM test GROUP BY GROUPING SETS((y, m),(),y),ROLLUP(r,s) ORDER BY 1,2,3,4;"
        , "SELECT y, m, r, s, SUM(u) FROM test GROUP BY GROUPING SETS((y,m),(),y),GROUPING SETS((),r,(r,s)) ORDER BY 1,2,3,4;"
        , "SELECT pubname, bookcount FROM library WHERE (bookcount, pubnum) IN (SELECT COUNT(*), book.pubnum FROM book GROUP BY pubnum);"
        , "SELECT pub_name, book_count FROM library WHERE (book_count, pub_num) IN (SELECT COUNT(*), pub_num FROM book GROUP BY pub_num) OR NOT IN (SELECT book.pub_num FROM book GROUP BY pub_num) AND book_count = 0;"
        , "SELECT emp_no, name, job_title, salary, yrs_exp FROM employee WHERE (salary,yrs_exp) >= ALL (SELECT salary,yrs_exp FROM employee);"
        , "SELECT name, dept_no, salary FROM employee WITH SUM(salary) BY dept_no;"
        , "SELECT name, dep_tno, salary FROM employee WITH SUM(salary) BY dept_no WITH SUM(salary);"
        , "SELECT name, dept_no, salary FROM employee ORDER BY name WITH SUM(salary) BY dept_no WITH SUM(salary);"
        , "SELECT name, dept_no, salary FROM employee WITH SUM(salary) BY dept_no WITH SUM(salary) ORDER BY name;"
        , "SELECT dept_no, SUM(salary) FROM employee GROUP BY dept_no WITH SUM(salary);"
        , "SELECT dept_no, SUM(salary) FROM employee GROUP BY dept_no WITH SUM(salary) BY dept_no;"
        , "SELECT SUM(amount) FROM sales_table AS s WITH AVG(amount) BY (SELECT prod_name FROM prod_table AS p WHERE p.prod_no = s.prod_no);"
        , "SELECT employee.dept_no, AVG(salary) FROM employee, department WHERE employee.dept_no = department.dept_no ORDER BY department.dept_name GROUP BY employee.dept_no;"
        ]

    , "Parse dialect-specific statements:" ~: map (TestCase . parsesSuccessfully)
        [ "ALTER TABLE foo RENAME TO bar;"
        , "ALTER TABLE foo, baz RENAME TO bar, quux;"
        , "ALTER TABLE foo SET SCHEMA other_schema;"
        , "ALTER TABLE foo SET SCHEMA other_schema RESTRICT;"
        , "ALTER TABLE foo SET SCHEMA other_schema CASCADE;"
        , "ALTER TABLE foo ADD PRIMARY KEY (bar);"
        , "ALTER TABLE foo ADD CONSTRAINT \"my_constraint\" PRIMARY KEY (bar, baz);"
        , "ALTER TABLE foo ADD FOREIGN KEY (a, b) REFERENCES bar;"
        , "ALTER TABLE foo ADD FOREIGN KEY (a, b) REFERENCES bar (c, d);"
        , "ALTER TABLE foo ADD CONSTRAINT foo_key_UK UNIQUE (foo_key);"
        , "ALTER PROJECTION foo RENAME TO bar;"
        , "SET TIME ZONE TO DEFAULT;"
        , "SET TIME ZONE TO 'PST8PDT';"
        , "SET TIME ZONE TO 'Europe/Rome';"
        , "SET TIME ZONE TO '-7';"
        , "SET TIME ZONE TO INTERVAL '-08:00 HOURS';"
        -- grants
        , "GRANT SELECT, REFERENCES ON TABLE foo TO bar WITH GRANT OPTION;", "GRANT SELECT ON ALL TABLES IN SCHEMA foo TO bar;"
        , "GRANT AUTHENTICATION v_gss to DBprogrammer;"
        , "GRANT EXECUTE ON PROCEDURE tokenize(varchar) TO Bob, Jules, Operator;"
        , "GRANT USAGE ON RESOURCE POOL Joe_pool TO Joe;"
        , "GRANT appdata TO bob;"
        , "GRANT USAGE ON SCHEMA online_sales TO Joe;"
        , "GRANT ALL PRIVILEGES ON SEQUENCE my_seq TO Joe;"
        , "GRANT ALL ON LOCATION '/home/dbadmin/UserStorage/BobStore' TO Bob;"
        , "GRANT EXECUTE ON SOURCE ExampleSource() TO Alice;", "GRANT ALL ON TRANSFORM FUNCTION Pagerank(varchar) to dbadmin;"
        , "GRANT ALL PRIVILEGES ON ship TO Joe;"
        -- revokes
        , "REVOKE ALL ON TABLE foo FROM bar CASCADE;" , "REVOKE GRANT OPTION FOR ALL PRIVILEGES ON TABLE foo FROM bar CASCADE;"
        , "REVOKE AUTHENTICATION localpwd from Public;"
        , "REVOKE TEMPORARY ON DATABASE vmartdb FROM Fred;"
        , "REVOKE EXECUTE ON tokenize(varchar) FROM Bob;"
        , "REVOKE USAGE ON RESOURCE POOL Joe_pool FROM Joe;"
        , "REVOKE ADMIN OPTION FOR pseudosuperuser FROM dbadmin;"
        , "REVOKE USAGE ON SCHEMA online_sales FROM Joe;"
        , "REVOKE ALL PRIVILEGES ON SEQUENCE my_seq FROM Joe;"
        , "REVOKE ALL ON LOCATION '/home/dbadmin/UserStorage/BobStore' ON v_mcdb_node0007 FROM Bob;"
        , "REVOKE ALL ON TRANSFORM FUNCTION Pagerank (float) FROM Doug;"
        , "REVOKE ALL PRIVILEGES ON ship FROM Joe;"
        -- functions
        , "SHOW search_path;"
        , "SELECT * FROM foo INNER JOIN bar USING (a);"
        , "SELECT datediff(qq, 'a', 'b');"
        , "SELECT datediff('qq', 'a', 'b');"
        , "SELECT datediff((SELECT 'qq'), 'a', 'b');"
        , "SELECT cast('1 minutes' AS INTERVAL MINUTE);"
        , "SELECT date_trunc('week', foo.at) FROM foo;"
        , "SELECT foo::TIME FROM bar;"
        , "SELECT 4 mod 3;"
        , "DROP VIEW foo.bar;"
        , "DROP VIEW IF EXISTS foo.bar;"
        ]

    , "Exclude some broken examples" ~: map (TestCase . parsesUnsuccessfully)
        [ "SELECT CURRENT_TIME();"
        -- TODO - this should not parse but currently does: , "SELECT 1 ESCAPE;"
        , "SELECT LOCALTIMESTAMP();"
        , "SELECT 'foo' ~~ 'bar' ESCAPE '!';"
        , "SELECT datediff(x, '1977-05-25', now());"
        , "SELECT datediff('a', 'b');"
        , "SELECT * FROM (foo);"
        , "SELECT 1 == 2;"
        , "SELECT * FROM (SELECT 1);"
        , "SELECT * FROM ((SELECT 1) as a);"
        , "SELECT * FROM (foo) as a;"
        , "SELECT 1 (a, b);"
        , "ALTER TABLE a, b RENAME TO c;"
        , "ALTER TABLE foo SET SCHEMA other_schema CASCADE RESTRICT;"
        , "GRANT;"
        , "REVOKE;"
        , "SHOW;"
        , "EXPLAIN TRUNCATE foo;"
        , "INSERT INTO foo VALUES (1,2), (3,4);"
        , "" -- the only way to write the empty statement is `;`
        , "\n" -- the only way to write the empty statement is `;`
        , TL.unlines
          [ "COPY public.foo(a, t as '2016-10-09T17-13-14.594')"
          , "SOURCE HDFS(url='http://host:port/webhdfs/v1/path/*')"
          , "null as 'null'"
          , "DIRECT"
          , "DIRECT" -- the options may not be repeated
          , "ENCLOSED BY '\"'"
          , "SKIP 1"
          , "DELIMITER E'\037'"
          , "RECORD TERMINATOR E'\036\012'"
          , "REJECTMAX 4096"
          , ";"
          ]
        , "SELECT 1 OR;"
        , "SELECT 1 AND;"
        -- may not have 'include/exclude schema privileges'
        , "WITH x AS (SELECT 1 AS a FROM dual)  SELECT * FROM X ORDER BY a  UNION  SELECT * FROM X ORDER BY a ;"
        , "WITH x AS (SELECT 1 AS a FROM dual)  SELECT * FROM X ORDER BY a  UNION  SELECT * FROM X            ;"
        , "MERGE INTO foo.bar t1 USING foo.baz t2 ON t1.a = t2.a"
        ]


    , "Parse exactly" ~:

        [ parse ";" ~?= Right
            (TeradataStandardSqlStatement (EmptyStmt (Range (Position 1 0 0) (Position 1 1 1))))

        , parse "SELECT 1" ~?= Right
          -- semicolon is optional
            ( TeradataStandardSqlStatement
                ( QueryStmt
                    ( QuerySelect (Range (Position 1 0 0) (Position 1 8 8))
                        Select
                            { selectInfo = Range (Position 1 0 0) (Position 1 8 8)
                            , selectCols = SelectColumns
                                { selectColumnsInfo = Range (Position 1 7 7) (Position 1 8 8)
                                , selectColumnsList = [
                                      SelectExpr
                                      ( Range (Position 1 7 7) (Position 1 8 8) )
                                      [ ColumnAlias
                                         (Range (Position 1 7 7) (Position 1 8 8))
                                         "?column?" (ColumnAliasId 1)
                                      ]
                                      ( ConstantExpr
                                          (Range (Position 1 7 7) (Position 1 8 8))
                                          (NumericConstant (Range (Position 1 7 7) (Position 1 8 8)) "1")
                                      )
                                      ]
                                }
                            , selectFrom = Nothing
                            , selectWhere = Nothing
                            , selectTimeseries = Nothing
                            , selectGroup = Nothing
                            , selectHaving = Nothing
                            , selectNamedWindow = Nothing
                            , selectDistinct = notDistinct
                            }
                    )
                )
            )


        , parse "SELECT foo FROM bar INNER JOIN baz ON 'true';" ~?= Right
            ( TeradataStandardSqlStatement
                ( QueryStmt
                    ( QuerySelect (Range (Position 1 0 0) (Position 1 44 44))
                        Select
                            { selectInfo = Range (Position 1 0 0) (Position 1 44 44)
                            , selectDistinct = notDistinct
                            , selectCols = SelectColumns
                                (Range (Position 1 7 7) (Position 1 10 10))
                                [ SelectExpr
                                    (Range (Position 1 7 7) (Position 1 10 10))
                                    [ColumnAlias (Range (Position 1 7 7) (Position 1 10 10)) "foo" (ColumnAliasId 1)]
                                    (ColumnExpr (Range (Position 1 7 7)
                                                       (Position 1 10 10))
                                            (QColumnName
                                                (Range (Position 1 7 7) (Position 1 10 10))
                                                Nothing
                                                "foo"))
                                ]

                            , selectFrom = Just $ SelectFrom (Range (Position 1 11 11)
                                                                    (Position 1 44 44))
                                [ TablishJoin (Range (Position 1 16 16)
                                                     (Position 1 44 44))

                                    Unused

                                    (JoinInner (Range (Position 1 20 20)
                                                      (Position 1 30 30)))

                                    (JoinOn (ConstantExpr (Range (Position 1 38 38)
                                                                 (Position 1 44 44))
                                                          (StringConstant (Range (Position 1 38 38)
                                                                                 (Position 1 44 44))
                                                                          "true")))
                                    (TablishTable
                                        (Range (Position 1 16 16)
                                               (Position 1 19 19))
                                        TablishAliasesNone
                                        (QTableName
                                            (Range (Position 1 16 16) (Position 1 19 19))
                                            Nothing
                                            "bar"))

                                    (TablishTable
                                        (Range (Position 1 31 31)
                                               (Position 1 34 34))
                                        TablishAliasesNone
                                        (QTableName
                                            (Range (Position 1 31 31) (Position 1 34 34))
                                            Nothing
                                            "baz"))
                                ]
                            , selectWhere = Nothing
                            , selectTimeseries = Nothing
                            , selectGroup = Nothing
                            , selectHaving = Nothing
                            , selectNamedWindow = Nothing
                            }
                    )
                )
            )
        , parse "SELECT 1 LIMIT 1;" ~?= Right
            ( TeradataStandardSqlStatement
                ( QueryStmt
                    ( QueryLimit
                        ( Range (Position 1 0 0) (Position 1 16 16) )
                        ( Limit
                            ( Range (Position 1 9 9) (Position 1 16 16) )
                            "1"
                        )
                        ( QuerySelect
                            ( Range (Position 1 0 0) (Position 1 8 8) )
                            ( Select
                                { selectInfo = Range (Position 1 0 0) (Position 1 8 8)
                                , selectCols = SelectColumns
                                    { selectColumnsInfo = Range (Position 1 7 7) (Position 1 8 8)
                                    , selectColumnsList =
                                        [ SelectExpr
                                            ( Range (Position 1 7 7) (Position 1 8 8) )
                                            [ ColumnAlias
                                                ( Range (Position 1 7 7) (Position 1 8 8) )
                                                "?column?"
                                                ( ColumnAliasId 1 )
                                            ]
                                            ( ConstantExpr
                                                ( Range (Position 1 7 7) (Position 1 8 8) )
                                                ( NumericConstant
                                                    ( Range (Position 1 7 7) (Position 1 8 8) )
                                                    "1"
                                                )
                                            )
                                        ]
                                    }
                                    , selectFrom = Nothing
                                    , selectWhere = Nothing
                                    , selectTimeseries = Nothing
                                    , selectGroup = Nothing
                                    , selectHaving = Nothing
                                    , selectNamedWindow = Nothing
                                    , selectDistinct = notDistinct
                                }
                            )
                        )
                    )
                )
            )

        , parse "(SELECT 1 LIMIT 0) UNION (SELECT 2 LIMIT 1) LIMIT 1;" ~?= Right
            ( TeradataStandardSqlStatement
                ( QueryStmt
                    ( QueryLimit
                        ( Range (Position 1 19 19) (Position 1 51 51) )
                        ( Limit
                            ( Range (Position 1 44 44) (Position 1 51 51) )
                            "1"
                        )
                        ( QueryUnion
                            ( Range (Position 1 19 19) (Position 1 24 24) )
                            ( Distinct True )
                            Unused
                            ( QueryLimit
                                ( Range (Position 1 1 1) (Position 1 17 17) )
                                ( Limit
                                    ( Range (Position 1 10 10) (Position 1 17 17) )
                                    "0"
                                )
                                ( QuerySelect
                                    ( Range (Position 1 1 1) (Position 1 9 9) )
                                    ( Select
                                        { selectInfo = Range (Position 1 1 1) (Position 1 9 9)
                                        , selectCols = SelectColumns
                                            { selectColumnsInfo = Range (Position 1 8 8) (Position 1 9 9)
                                            , selectColumnsList =
                                                [ SelectExpr
                                                    ( Range (Position 1 8 8) (Position 1 9 9) )
                                                    [ ColumnAlias
                                                        ( Range (Position 1 8 8) (Position 1 9 9) )
                                                        "?column?"
                                                        ( ColumnAliasId 1 )
                                                    ]
                                                    ( ConstantExpr
                                                        ( Range (Position 1 8 8) (Position 1 9 9) )
                                                        ( NumericConstant
                                                            ( Range (Position 1 8 8) (Position 1 9 9) )
                                                            "1"
                                                        )
                                                    )
                                                ]
                                            }
                                        , selectFrom = Nothing
                                        , selectWhere = Nothing
                                        , selectTimeseries = Nothing
                                        , selectGroup = Nothing
                                        , selectHaving = Nothing
                                        , selectNamedWindow = Nothing
                                        , selectDistinct = notDistinct
                                        }
                                    )
                                )
                            )
                            ( QueryLimit
                                ( Range (Position 1 26 26) (Position 1 42 42) )
                                ( Limit
                                    ( Range (Position 1 35 35) (Position 1 42 42) )
                                    "1"
                                )
                                ( QuerySelect
                                    ( Range (Position 1 26 26) (Position 1 34 34) )
                                    ( Select
                                        { selectInfo = Range (Position 1 26 26) (Position 1 34 34)
                                        , selectCols = SelectColumns
                                            { selectColumnsInfo = Range (Position 1 33 33) (Position 1 34 34)
                                            , selectColumnsList =
                                                [ SelectExpr
                                                    ( Range (Position 1 33 33) (Position 1 34 34) )
                                                    [ ColumnAlias
                                                        ( Range (Position 1 33 33) (Position 1 34 34) )
                                                        "?column?"
                                                        ( ColumnAliasId 2 )
                                                    ]
                                                    ( ConstantExpr
                                                        ( Range (Position 1 33 33) (Position 1 34 34) )
                                                        ( NumericConstant
                                                            ( Range (Position 1 33 33) (Position 1 34 34) )
                                                            "2"
                                                        )
                                                    )
                                                ]
                                            }
                                        , selectFrom = Nothing
                                        , selectWhere = Nothing
                                        , selectTimeseries = Nothing
                                        , selectGroup = Nothing
                                        , selectHaving = Nothing
                                        , selectNamedWindow = Nothing
                                        , selectDistinct = notDistinct
                                        }
                                    )
                                )
                            )
                        )
                    )
                )
            )

        , parse "SELECT a, rank() OVER (PARTITION BY a ORDER BY 1) FROM foo ORDER BY 1;" ~?= Right
          ( TeradataStandardSqlStatement
            ( QueryStmt
              ( QueryOrder
                ( Range (Position 1 0 0) (Position 1 69 69) )
                [ Order
                  ( Range (Position 1 68 68) (Position 1 69 69) )
                  ( PositionOrExprPosition (Range (Position 1 68 68) (Position 1 69 69)) 1 Unused )
                  ( OrderAsc Nothing )
                  ( NullsAuto Nothing )
                ]
                ( QuerySelect
                  ( Range (Position 1 0 0) (Position 1 58 58) )
                  ( Select
                    { selectInfo = Range (Position 1 0 0) (Position 1 58 58)
                    , selectCols = SelectColumns
                        { selectColumnsInfo = Range (Position 1 7 7) (Position 1 49 49)
                        , selectColumnsList =
                            [ SelectExpr
                              ( Range (Position 1 7 7) (Position 1 8 8) )
                              [ ColumnAlias (Range (Position 1 7 7) (Position 1 8 8)) "a" (ColumnAliasId 1) ]
                              ( ColumnExpr
                                ( Range (Position 1 7 7) (Position 1 8 8) )
                                ( QColumnName (Range (Position 1 7 7) (Position 1 8 8)) Nothing "a" )
                              )
                            , SelectExpr
                              ( Range (Position 1 10 10) (Position 1 49 49) )
                              [ ColumnAlias (Range (Position 1 10 10) (Position 1 49 49)) "rank" (ColumnAliasId 2) ]
                              ( FunctionExpr
                                ( Range (Position 1 10 10) (Position 1 49 49) )
                                ( QFunctionName (Range (Position 1 10 10) (Position 1 14 14)) Nothing "rank" )
                                (Distinct False)
                                []
                                []
                                Nothing
                                ( Just
                                  ( OverWindowExpr
                                    ( Range (Position 1 17 17) (Position 1 49 49) )
                                    ( WindowExpr
                                      { windowExprInfo = Range (Position 1 17 17) (Position 1 49 49)
                                      , windowExprPartition = Just
                                          ( PartitionBy
                                            ( Range (Position 1 23 23) (Position 1 37 37) )
                                            [ ColumnExpr
                                              ( Range (Position 1 36 36) (Position 1 37 37) )
                                              ( QColumnName (Range (Position 1 36 36) (Position 1 37 37)) Nothing "a" )
                                            ]
                                          )
                                      , windowExprOrder =
                                        [ Order
                                          ( Range (Position 1 47 47) (Position 1 48 48) )
                                          ( PositionOrExprExpr
                                            ( ConstantExpr
                                              ( Range (Position 1 47 47) (Position 1 48 48) )
                                              ( NumericConstant
                                               ( Range (Position 1 47 47) (Position 1 48 48) )
                                               "1"
                                              )
                                            )
                                          )
                                          (OrderAsc Nothing)
                                          (NullsLast Nothing)
                                        ]
                                      , windowExprFrame = Nothing
                                      }))))]}
                    , selectFrom = Just
                        ( SelectFrom
                          ( Range (Position 1 50 50) (Position 1 58 58) )
                          [ TablishTable
                            ( Range (Position 1 55 55) (Position 1 58 58) )
                            TablishAliasesNone
                            ( QTableName (Range (Position 1 55 55) (Position 1 58 58)) Nothing "foo" )])
                    , selectWhere = Nothing
                    , selectTimeseries = Nothing
                    , selectGroup = Nothing
                    , selectHaving = Nothing
                    , selectNamedWindow = Nothing
                    , selectDistinct = Distinct False
                    })))))

        , parse "SELECT created_at AT TIMEZONE 'PST' > now();" ~?= Right
          ( TeradataStandardSqlStatement
            ( QueryStmt
              ( QuerySelect
                ( Range (Position 1 0 0) (Position 1 37 37) )
                ( Select
                  { selectInfo = Range (Position 1 0 0) (Position 1 37 37)
                  , selectCols = SelectColumns
                    { selectColumnsInfo = Range (Position 1 36 36) (Position 1 37 37)
                    , selectColumnsList =
                      [ SelectExpr
                        ( Range (Position 1 36 36) (Position 1 37 37) )
                        [ ColumnAlias
                          ( Range (Position 1 36 36) (Position 1 37 37) )
                          "?column?"
                          ( ColumnAliasId 1 )
                        ]
                        ( BinOpExpr
                          ( Range (Position 1 36 36) (Position 1 37 37))
                          ( Operator ">")
                          ( AtTimeZoneExpr
                            ( Range (Position 1 7 7) (Position 1 35 35) )
                            ( ColumnExpr
                              ( Range (Position 1 7 7) (Position 1 17 17) )
                              ( QColumnName
                                { columnNameInfo = Range (Position 1 7 7) (Position 1 17 17)
                                , columnNameTable = Nothing, columnNameName = "created_at"
                                }
                              )
                            )
                            ( ConstantExpr
                                ( Range (Position 1 30 30) (Position 1 35 35) )
                                ( StringConstant
                                  ( Range (Position 1 30 30) (Position 1 35 35) )
                                  "PST"
                                )
                            )
                          )
                          ( FunctionExpr
                            ( Range (Position 1 38 38) (Position 1 43 43) )
                            ( QFunctionName
                              { functionNameInfo = Range (Position 1 38 38) (Position 1 41 41)
                              , functionNameSchema = Nothing
                              , functionNameName = "now"
                              }
                            )
                            (Distinct False)
                            []
                            []
                            Nothing
                            Nothing
                          )
                        )
                      ]
                    }
                  , selectFrom = Nothing
                  , selectWhere = Nothing
                  , selectTimeseries = Nothing
                  , selectGroup = Nothing
                  , selectHaving = Nothing
                  , selectNamedWindow = Nothing
                  , selectDistinct = Distinct False
                  }
                )
              )
            )
          )
        ]
    ]

tests :: Test
tests = test [ testParser ]

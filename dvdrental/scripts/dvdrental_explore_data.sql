---PROJECT: DVDRENTAL ANALYSIS
---AUTHOR: MARIAM LAWAL
---DATE: JAN 2026
--TASK 1
--Script list the first 10 film titles in alphabetical order
SELECT title
FROM film
ORDER BY title ASC
LIMIT 10;

--Script list the titles and release years of all films released in 2006, sorted by title.
SELECT title, release_year
FROM film
WHERE release_year = 2006
ORDER BY title DESC;

--Script list the titles and ratings of all films rated PG or PG-13
--sorted by title in reverse alphabetical order. 
SELECT title, rating
FROM film
WHERE rating IN ('PG', 'PG-13')
ORDER BY title DESC;

--Script show all the distinct film ratings in the database. 
SELECT DISTINCT rating
FROM film;

--Script find the total number of films in the database. 
SELECT COUNT(title)
FROM film;

--Script show each rating, show the number of films, sorted by the largest count first.
SELECT rating, COUNT(rating) AS Rating_Total
FROM film
GROUP BY rating
ORDER BY Rating_Total DESC;

--Script show each rating, show the number of films only if there are at least 200 films in that 
--rating. 
SELECT rating, COUNT(rating) AS Rating_Total
FROM film
GROUP BY rating
HAVING COUNT(rating) >= 200
ORDER BY Rating_Total DESC;

--Script list the 10 longest films with their titles and lengths, starting with the longest. 
SELECT title, length
FROM film
ORDER BY length DESC
LIMIT 10;

--Script show each film category and the number of films in it, sorted from largest to 
--smallest. 
SELECT c.category_id, c.name, COUNT(f.film_id) AS Total_film
FROM category c
INNER JOIN film_category fc ON c.category_id = fc.category_id
INNER JOIN film f ON f.film_id = fc.film_id
GROUP BY c.category_id, c.name
ORDER BY Total_film DESC;

--Script show the 10 most frequently rented films with their rental counts.
SELECT f.title, COUNT(r.rental_id) AS Total_rentals
FROM film f
INNER JOIN inventory i ON f.film_id = i.film_id
INNER JOIN rental r ON i.inventory_id = r.inventory_id
GROUP BY f.title
ORDER BY Total_rentals DESC
LIMIT 10;

--Script show each customer’s full name and the number of rentals they have made, sorted by highest number first.
SELECT CONCAT(c.first_name, ' ', c.last_name) AS Fullname, 
       COUNT(r.rental_id) AS Total_rentals
FROM customer c
LEFT JOIN rental r ON c.customer_id = r.customer_id
GROUP BY Fullname
ORDER BY Total_rentals DESC;

--Script show each customer’s full name, total amount paid, and number of payments they have made, sorted by the highest total paid first.
SELECT CONCAT(c.first_name, ' ', c.last_name) AS Fullname,
       SUM(amount) AS Total_payment,
       COUNT(p.payment_id) AS Count_payment
FROM customer c
LEFT JOIN payment p ON c.customer_id = p.customer_id
GROUP BY Fullname
ORDER BY Total_payment DESC;

--Script list all customers who live in Canada with their city and email address.
SELECT c.first_name, c.last_name, a.address, ci.city, c.email
FROM customer c
INNER JOIN address a ON c.address_id = a.address_id 
INNER JOIN city ci ON a.city_id = ci.city_id
INNER JOIN country co ON ci.country_id = co.country_id
WHERE co.country = 'Canada';

--Script show the average payment amount for each month, with the newest month first.
SELECT TO_CHAR(payment_date, 'YYYY-MM') AS Monthly_Payment,
       AVG(amount) AS Average_Payment
FROM payment
GROUP BY TO_CHAR(payment_date, 'YYYY-MM')
ORDER BY Monthly_Payment DESC;

--Script show each film’s title and the number of actors in it,
--only if the film has at least 5 actors.
SELECT f.title, COUNT(fa.actor_id) AS Total_actor
FROM film f
INNER JOIN film_actor fa ON f.film_id = fa.film_id
GROUP BY f.title
HAVING COUNT(fa.actor_id) >= 5;

--Script show each store and the total payment amount it has processed,
--sorted from highest to lowest.
SELECT s.store_id, SUM(p.amount) AS Totalamount_processed
FROM store s
INNER JOIN staff st ON st.store_id = s.store_id
INNER JOIN payment p ON st.staff_id = p.staff_id
GROUP BY s.store_id
ORDER BY Totalamount_processed DESC;

--TASK 2
--Script find the top 5 customers by total payment amount.
SELECT c.customer_id, c.first_name, c.last_name, 
       SUM(p.amount) AS Totalamount_payment
FROM customer c
INNER JOIN payment p ON c.customer_id = p.customer_id
GROUP BY c.customer_id
ORDER BY Totalamount_payment DESC
LIMIT 5;

--Script identify the top 3 most rented film categories.
SELECT c.name AS Category_name, COUNT(r.rental_id) AS Total_rental
FROM category c
INNER JOIN film_category fc ON c.category_id = fc.category_id
INNER JOIN film f ON fc.film_id = f.film_id
INNER JOIN inventory i ON f.film_id = i.film_id
INNER JOIN rental r ON i.inventory_id = r.inventory_id
GROUP BY c.name
ORDER BY Total_rental DESC
LIMIT 3;

--Script calculate the average rental duration per film category.
SELECT fc.category_id, c.name, AVG(f.rental_duration) AS Average_rentalduration
FROM film_category fc
INNER JOIN category c ON c.category_id = fc.category_id
INNER JOIN film f ON f.film_id = fc.film_id
GROUP BY fc.category_id, c.name
ORDER BY Average_rentalduration DESC;

--Script show each store monthly revenue for the most recent year in the dataset
SELECT s.store_id, TO_CHAR(p.payment_date, 'YYYY-MM') AS Month,
       SUM(p.amount) AS Monthly_revenue
FROM payment p
INNER JOIN staff st ON p.staff_id = st.staff_id
INNER JOIN store s ON st.store_id = s.store_id
WHERE EXTRACT(YEAR FROM p.payment_date) = (
      SELECT MAX(EXTRACT(YEAR FROM payment_date))
	  FROM payment
)
GROUP BY s.store_id, TO_CHAR(p.payment_date, 'YYYY-MM')
ORDER BY s.store_id, Month;

--Script find films that were never rented.
SELECT f.film_id, f.title, r.rental_id
FROM film f
LEFT JOIN inventory i ON f.film_id = i.film_id
LEFT JOIN rental r ON i.inventory_id = r.inventory_id
WHERE rental_id IS NULL;

--Script list of customers who have spent over $100 in total.
SELECT c.customer_id, c.first_name, c.last_name, 
       SUM(p.amount) AS Totalamount_spent
FROM customer c
INNER JOIN payment p ON c.customer_id = p. customer_id
GROUP BY c.customer_id
HAVING SUM(p.amount) > 100
ORDER BY Totalamount_spent DESC;

--Script show the staff members with the highest total payments processed.
SELECT s.staff_id, s.first_name, s.last_name, 
       SUM(p.amount) AS Totalpayment_processed
FROM staff s
INNER JOIN payment p ON p.staff_id = s.staff_id
GROUP BY s.staff_id
LIMIT 1;

--Script show each category, average film length, only including categories with above-average length.
SELECT c.name, AVG(f.length) AS Averagefilm_length
FROM category c
INNER JOIN film_category fc ON fc.category_id = c.category_id
INNER JOIN film f ON f.film_id = fc.film_id
GROUP BY c.category_id, c.name
HAVING AVG(f.length) > (SELECT AVG(length)
                        FROM film);





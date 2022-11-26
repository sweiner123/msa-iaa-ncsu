-- 1. How many records are in the movies table?

select count(*)
from movies;
-- 36878

-- 2. How many DVDs cost more than $20?

select count(*)
from movies
where dvd_price > 20.0;

-- 13271

-- Create a report to find the people who have the first name 'Francisco'? List their
-- ID numbers and names. Hint: use the following condition in the where clause:

select id, name
from people
where name like 'Francisco%';

-- 2 people

select pp.name, m.movie_name
from people pp,
     people_movies pm,
     movies m
where pp.id = pm.person_id
  and pm.movie_id = m.id
order by 1;

-- 5. Provide a report that lists the person name and the count of movies owned by
-- person. Provide the count of movies in descending order
select pp.name, count(*) as Count_Movies
from people pp,
     people_movies pm,
     movies m
where pp.id = pm.person_id
  and pm.movie_id = m.id
group by pp.name
order by 2 desc;

-- 6. What is the average DVD price for each movie rating? - Do not ignore missing
-- values for the ratings.

select rating, round(avg(dvd_price), 2) as Avg_DVD_Price
from movies
group by rating;

-- 7. List the ratings that have an average DVD price greater than 20. Do not ignore
-- missing values for the ratings

select rating, round(avg(dvd_price), 2) as Avg_DVD_Price
from movies
group by rating
having Avg_DVD_Price > 20;

-- 8. What is the grand total cost of all of the movies listed in the movies table?

select sum(dvd_price)
from movies;
-- 982712.7799995932

-- 9. We want to reduce the price of each DVD by half. Create a report that lists the
-- movie id, its current DVD price, and the new price

select id, dvd_price as Current_DVD_Price, 0.5 * movies.dvd_price as New_Price
from movies;

-- 10. How many movies in the list are not owned by anyone?

select count(*)
from (select id as MOVIE_ID
      from movies
      except
      select m.id as MOVIE_ID
      from people pp,
           people_movies pm,
           movies m
      where pp.id = pm.person_id
        and pm.movie_id = m.id) as t;

-- 6181

select count(*)
from (select id
      from movies
      except
      select movie_id
      from people_movies);

-- 6181

-- 11. Create a NEW table that lists the name of the studios, and their respective counts
-- of movies.

create table if not exists studio_table
as
select studios.studio_name, count(movies.id) as Movie_Count
from movies,
     studios
where movies.studio_id = studios.id
group by studios.studio_name;

-- 12. Insert a new person into the People table named "Jack Smith" with ID=3000

INSERT into people
values (3000, "Jack Smith");

select count(*)
from people;
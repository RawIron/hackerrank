/*
    projects are listed in a tasks table
    with two columns

    A project started on day 1 and ended
    on day 5:
        start       end
         1           2
         2           3
         3           4
         4           5

    tasks timeline
    1        10                  30
    +---+----+----+----+----+----+
    ^     ^^^ ^        ^   ^     ^
      p1   p2     p3         p4
      
    projects never overlap

    project which only lasted 1 day
        7-8
    project which lasted more than 1 day
        1-2-2-3-3-4-4-5
    and because projects never overlap
    it is safe to collapse the above to
        1-5

    after all more than 1 day projects
    have been collapsed and the column
    is sorted it looks like
        1-7-8-9-11-20-24-30
    now each pair is a start and end day
    of a project

    Database: MySQL 5.x
*/
set @pairnum := 1;

create table tt_paired
select
    project_day as project,
    (@pairnum := @pairnum + 1) DIV 2 as pair_id
from (
    select project_day
    from (
        select start_date as project_day
        from projects
        union all
        select end_date as project_day
        from projects
    ) as single_date_column
    group by project_day
    having count(*) = 1
    order by project_day asc
) as paired
;

select
    start_day.project,
    end_day.project
from tt_paired as start_day
inner join tt_paired as end_day
    on start_day.pair_id = end_day.pair_id
    and start_day.project < end_day.project
order by
    (end_day.project - start_day.project),
     start_day.project
;
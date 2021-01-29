SELECT
    hacker_id,
    name,
    totalscore
FROM (
    SELECT
        hacker_id,
        SUM(maxscore) AS totalscore
    FROM (
        SELECT
            hacker_id,
            challenge_id,
            MAX(score) AS maxscore
        FROM Submissions
        GROUP BY hacker_id, challenge_id
    ) AS maxscores
    GROUP BY hacker_id
) AS totalscores
INNER JOIN Hackers USING(hacker_id)
WHERE totalscore > 0
ORDER BY totalscore DESC, hacker_id ASC

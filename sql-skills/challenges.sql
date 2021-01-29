/*
 * mysql 5.7 does not support WITH
 */
SELECT hacker_id, name, challenges_created
FROM (
    SELECT hacker_id, name, COUNT(challenge_id) AS challenges_created
    FROM Hackers INNER JOIN Challenges USING (hacker_id)
    GROUP BY hacker_id, name
    ) AS challenges_total0
WHERE challenges_created NOT IN (
    SELECT
        challenges_created
    FROM (
        SELECT hacker_id, COUNT(challenge_id) AS challenges_created
        FROM Challenges
        GROUP BY hacker_id
        ) AS challenges_total1
    WHERE challenges_created < (
        SELECT MAX(challenges_created)
        FROM (
            SELECT hacker_id, COUNT(challenge_id) AS challenges_created
            FROM Challenges
            GROUP BY hacker_id
            ) as challenges_total2
        )
    GROUP BY challenges_created
    HAVING COUNT(hacker_id) > 1
    )
ORDER BY challenges_created DESC, hacker_id ASC

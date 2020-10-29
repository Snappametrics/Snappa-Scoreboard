SELECT 
player_id, COUNT(player_id) as games_played, sum(shots) as total_shots, sum(total_points) as total_points, sum(ones) as ones, sum(twos) as twos, sum(threes) as threes, sum(impossibles) as impossibles, 
sum(paddle_points) as paddle_points, sum(clink_points) as clink_points, 
sum(total_points)/sum(shots) as points_per_game, (sum(total_points)-sum(paddle_points))/sum(shots) as off_ppr, sum(paddle_points)/sum(shots) as def_ppr, 
FROM public.player_stats
GROUP BY player_id
ORDER BY player_id;
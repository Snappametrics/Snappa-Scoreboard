SELECT ps.game_id, gs.night_dice, gs.threes as sinks, gs.paddle_points as paddles, ps.team, players.player_name, ps.total_points, ps.shots, ps.ones, ps.twos, ps.threes, ps.impossibles, ps.paddle_points, ps.clink_points
FROM players
LEFT JOIN player_stats as ps ON players.player_id = ps.player_id
LEFT JOIN game_stats as gs ON ps.game_id = gs.game_id
WHERE ps.game_id = (select max(game_id) from player_stats)
ORDER BY ps.total_points desc;
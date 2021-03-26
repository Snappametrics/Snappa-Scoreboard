CREATE TABLE scores_assists (
	game_id integer,
	rally_id integer,
	score_id integer,
	player_id integer,
	scoring_team varchar(1),
	round_num varchar(3),
	points_scored integer,
	paddle boolean,
	hand boolean,
	head boolean,
	foot boolean,
	clink boolean,
	CONSTRAINT unique_score PRIMARY KEY (game_id, rally_id, score_id)
);

CREATE TABLE assists (
	game_id integer,
	round_num varchar(3),
	rally_id integer,
	score_id integer,
	touch_id integer,
	player_id integer,
	team varchar(1),
	paddle boolean,
	assist boolean,
	hand boolean,
	head boolean,
	foot boolean,
	CONSTRAINT unique_touch PRIMARY KEY(game_id, rally_id, score_id, touch_id)
)

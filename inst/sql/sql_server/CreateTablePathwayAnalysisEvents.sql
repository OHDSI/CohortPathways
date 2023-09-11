CREATE TABLE @pathway_analysis_events
(
	pathway_analysis_generation_id BIGINT NOT NULL,
	target_cohort_id INTEGER NOT NULL,
	combo_id BIGINT NOT NULL,
	subject_id BIGINT NOT NULL,
	ordinal INTEGER,
	cohort_start_date DATETIME NOT NULL,
	cohort_end_date DATETIME NOT NULL
);

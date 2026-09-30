-- migrate:up

-- rate_limits.window_start holds the window start as epoch seconds. As REAL
-- (float4) it could only represent multiples of 128 seconds at current
-- epochs, so the 60-second window snapped to 128-second steps and every step
-- boundary reset a bucket's count mid-window: five attempts just before a
-- boundary and five just after all passed. double precision keeps sub-second
-- resolution. Existing rows keep their (already rounded) values.
ALTER TABLE rate_limits ALTER COLUMN window_start TYPE double precision;

-- migrate:down

ALTER TABLE rate_limits ALTER COLUMN window_start TYPE real;

-- postgresql examples

-- fuction str_endswith
CREATE OR REPLACE FUNCTION str_endswith(str TEXT, suffix TEXT) RETURNS bool AS
$$
DECLARE
    s0_len INTEGER := 0;
    s1_len INTEGER := 0;
BEGIN
    s0_len := length(str);
    s1_len := length(suffix);
    RETURN (s0_len >= s1_len) and (substr(str, s0_len - s1_len + 1) = suffix);
END
$$
LANGUAGE PLPGSQL;

-- operator for str_endswith
CREATE OPERATOR !# (PROCEDURE = str_endswith,
                    LEFTARG = TEXT,
                    RIGHTARG = TEXT);

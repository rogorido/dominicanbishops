

CREATE OR REPLACE TEMP VIEW tv_paises_todos as
SELECT DISTINCT country
FROM places
WHERE country IS NOT NULL ORDER BY country;

CREATE OR REPLACE TEMP VIEW tv_paises_europa AS
SELECT DISTINCT country
FROM places
WHERE country is NOT NULL AND 'Europe' = ANY(macroregions);

CREATE OR REPLACE TEMP VIEW tv_paises_noeuropa AS
SELECT DISTINCT country
FROM places
WHERE country is NOT NULL AND NOT 'Europe' = ANY(macroregions);

CREATE OR REPLACE TEMP VIEW tv_paises_bizancio AS
SELECT DISTINCT country
FROM places
WHERE country IN 
 ('Greece', 'Ucraine', 'Romania', 'Bulgaria', 'Albania', 'Slovenia',
  'Croacia', 'Serbia', 'Montenegro', 'Cyprus', 'Lebanon', 'Israel', 'Palestina', 'Turkey' );

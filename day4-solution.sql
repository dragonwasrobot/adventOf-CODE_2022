-- $ sqlite3 advent-of-code.db
CREATE TABLE day4 (
  left_elf VARCHAR(50) NOT NULL,
  right_elf VARCHAR(50) NOT NULL
);

.separator ","
.import "day4-input.txt" day4

ALTER TABLE day4 ADD COLUMN left_elf_start INTEGER;
ALTER TABLE day4 ADD COLUMN left_elf_stop INTEGER;
ALTER TABLE day4 ADD COLUMN right_elf_start INTEGER;
ALTER TABLE day4 ADD COLUMN right_elf_stop INTEGER;

UPDATE day4 SET left_elf_start = SUBSTR(left_elf, 1, INSTR(left_elf, '-') - 1);
UPDATE day4 SET left_elf_stop = SUBSTR(left_elf, INSTR(left_elf, '-') + 1);
UPDATE day4 SET right_elf_start = SUBSTR(right_elf, 1, INSTR(right_elf, '-') - 1);
UPDATE day4 SET right_elf_stop = SUBSTR(right_elf, INSTR(right_elf, '-') + 1);

-- Solution 1
SELECT COUNT(*)
  FROM day4
  WHERE (left_elf_start >= right_elf_start AND
         left_elf_stop <= right_elf_stop)
  OR (right_elf_start >= left_elf_start AND
      right_elf_stop <= left_elf_stop); -- 500

-- Solution 2
SELECT COUNT(*)
  FROM day4
  WHERE (left_elf_start <= right_elf_start AND
         left_elf_stop >= right_elf_start)
    OR (left_elf_start >= right_elf_start AND
        left_elf_start <= right_elf_stop); -- 815

SELECT
  s.district_no AS district,
  s.school_no AS school,
  ta.school_year + 1 AS school_year,
  ta.person_id,
  sm.license_number,
  ta.ta_begin_date,
  ta.ta_end_date
FROM
  (SELECT *
    FROM teacher_assignment
    WHERE school_year IN (2016, 2017, 2018, 2019) AND teacher_of_record = 'Y') ta
  JOIN staff_member sm ON ta.person_id = sm.person_id
  JOIN school s ON ta.school_bu_id = s.school_bu_id
ORDER BY s.district_no, s.school_no, ta.school_year, ta.person_id

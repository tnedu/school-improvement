SELECT
  s.district_no AS system,
  s.school_no AS school,
  isp.student_key,
  isp.isp_id,
  isp.begin_date,
  isp.end_date
FROM
  (SELECT *
    FROM instructional_service_period
    WHERE school_year = 2019 AND type_of_service = 'P') isp
  JOIN school s ON isp.school_bu_id = s.school_bu_id

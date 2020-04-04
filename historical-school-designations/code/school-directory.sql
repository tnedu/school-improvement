SELECT
  s.district_number AS district,
  bu2.bu_name AS district_name,
  s.school_number AS school,
  bu.bu_name AS school_name,
  bu.status,
  op.op_begin_date,
  op.op_end_date,
  s.school_type_id,
  st.st_description AS school_type,
  st.active AS school_type_active,
  s.instructional_type_id,
  it.it_description AS instructional_type,
  it.active AS instructional_type_active,
  sp.school_program_type_id,
  spt.spt_description AS school_program_type,
  spt.active AS school_program_type_active,
  sp_begin_date,
  sp_end_date
FROM
  (SELECT * FROM business_unit WHERE bu_type_id = '003') bu
  JOIN school s ON bu.bu_id = s.bu_id
  JOIN business_unit bu2 ON bu.parent_bu_id = bu2.bu_id
  LEFT JOIN operational_period op ON bu.bu_id = op.bu_id
  LEFT JOIN school_types st ON s.school_type_id = st.school_type_id
  LEFT JOIN instructional_types it ON s.instructional_type_id = it.instructional_type_id
  LEFT JOIN (
    SELECT *
    FROM school_program
    WHERE school_program_type_id IN ('021', '022', '024', '025', '026')
  ) sp ON bu.bu_id = sp.bu_id
  LEFT JOIN school_program_types spt ON sp.school_program_type_id = spt.school_program_type_id

select
  s.district_no as district,
  s.school_no as school,
  c.school_year + 1 as school_year,
  c.instructional_program_num as ipn,
  c.id_date
from
  (select * from school where school_bu_id in SCHOOL IDS HERE) s
  join district d on s.district_no = d.district_no
  join scal_id_days c on s.school_bu_id = c.school_bu_id
where c.school_year in (2016, 2017, 2018, 2019)

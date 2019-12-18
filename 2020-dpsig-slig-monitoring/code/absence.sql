select
  d.district_no as district,
  s.school_no as school,
  i.school_year + 1 as school_year,
  i.instructional_program_num as ipn,
  i.student_key as state_id,
  i.isp_id,
  i.begin_date,
  i.end_date,
  a.attendance_type as absence_type,
  a.attendance_date as absence_date
from
  (select * from school where school_bu_id in SCHOOL IDS HERE) s
  join district d on s.district_no = d.district_no
  join instructional_service_period i on s.school_bu_id = i.school_bu_id
  left join
    (select * from student_absences where attendance_type in ('A', 'T', 'U', 'X')) a
    on i.isp_id = a.isp_id
where
  i.type_of_service = 'P'
  and i.school_year in (2018, 2019)

CREATE VIEW patient_admissions AS
SELECT P.patient_id, P.name, A.admission_id, A.date_of_admission, A.date_of_discharge
FROM PATIENT P
JOIN ADMISSION A ON P.patient_id = A.patient_id;

CREATE VIEW doctor_specializations AS
SELECT D.doctor_id, D.name, D.specialization
FROM DOCTOR D;

CREATE VIEW doctor_appointments AS
SELECT D.doctor_id, D.name, A.date, A.time
FROM DOCTOR D
JOIN APPOINTMENT A ON D.doctor_id = A.doctor_id
JOIN PATIENT P ON A.patient_id = P.patient_id;

CREATE VIEW doctor_treatments AS
SELECT D.doctor_id, D.name, T.treatment_id, T.description
FROM DOCTOR D
JOIN TREATMENT T ON D.doctor_id = T.doctor_id;

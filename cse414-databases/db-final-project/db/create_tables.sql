CREATE TABLE PATIENT_TYPE (
    type VARCHAR(255) PRIMARY KEY,
    description TEXT
);

CREATE TABLE PATIENT (
    patient_id SERIAL PRIMARY KEY,
    name VARCHAR(255),
    address VARCHAR(255),
    date_of_birth DATE,
    gender CHAR(1),
    phone_number VARCHAR(15),
    type VARCHAR(255) REFERENCES PATIENT_TYPE(type)
);

CREATE TABLE DOCTOR (
    doctor_id SERIAL PRIMARY KEY,
    name VARCHAR(255),
    specialization VARCHAR(255),
    date_of_joining DATE,
    phone_number VARCHAR(15),
    department_name VARCHAR(255) REFERENCES DEPARTMENT(name)
);

CREATE TABLE SURGEON (
    doctor_id INT PRIMARY KEY REFERENCES DOCTOR(doctor_id) ON DELETE CASCADE;
);

CREATE TABLE GENERAL_PRACTITIONER (
    doctor_id INT PRIMARY KEY REFERENCES DOCTOR(doctor_id) ON DELETE CASCADE;
);

CREATE TABLE NURSE (
    nurse_id SERIAL PRIMARY KEY,
    name VARCHAR(255),
    date_of_joining DATE,
    phone_number VARCHAR(15)
);

CREATE TABLE ADMISSION (
    admission_id SERIAL PRIMARY KEY,
    date_of_admission DATE,
    date_of_discharge DATE,
    room_number VARCHAR(10),
    patient_id INT REFERENCES PATIENT(patient_id),
    doctor_id INT REFERENCES DOCTOR(doctor_id)
);

CREATE TABLE PRESCRIPTION (
    prescription_id SERIAL PRIMARY KEY,
    medicine_name VARCHAR(255),
    quantity INT,
    date_prescribed DATE,
    admission_id INT REFERENCES ADMISSION(admission_id)
);

CREATE TABLE MEDICINE (
    name VARCHAR(255) PRIMARY KEY,
    manufacturer VARCHAR(255),
    side_effects TEXT
);

CREATE TABLE APPOINTMENT (
    appointment_id SERIAL PRIMARY KEY,
    date DATE,
    time TIME,
    purpose TEXT,
    patient_id INT REFERENCES PATIENT(patient_id),
    doctor_id INT REFERENCES DOCTOR(doctor_id),
    department_name VARCHAR(255)
);

CREATE TABLE TREATMENT (
    treatment_id SERIAL PRIMARY KEY,
    description TEXT,
    doctor_id INT REFERENCES DOCTOR(doctor_id),
    patient_id INT REFERENCES PATIENT(patient_id),
    medicine_name VARCHAR(255) REFERENCES MEDICINE(name)
);

CREATE TABLE DEPARTMENT (
    name VARCHAR(255) PRIMARY KEY,
    location VARCHAR(255),
    hospital_id INT
);

CREATE TABLE HOSPITAL (
    hospital_id SERIAL PRIMARY KEY,
    name VARCHAR(255),
    address VARCHAR(255),
    phone_number VARCHAR(15)
);

ALTER TABLE DEPARTMENT ADD FOREIGN KEY (hospital_id) REFERENCES HOSPITAL(hospital_id);
ALTER TABLE APPOINTMENT ADD FOREIGN KEY (department_name) REFERENCES DEPARTMENT(name);

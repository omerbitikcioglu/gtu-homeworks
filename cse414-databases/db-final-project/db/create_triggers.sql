-- This function prevents the deletion of a patient who has upcoming appointments
CREATE OR REPLACE FUNCTION prevent_patient_deletion() RETURNS TRIGGER AS $$
DECLARE
  upcoming_appointments INT;
BEGIN
  -- Count the number of upcoming appointments for the patient
  SELECT COUNT(*) INTO upcoming_appointments
  FROM APPOINTMENT
  WHERE patient_id = OLD.patient_id AND date >= CURRENT_DATE;

  -- If the patient has upcoming appointments, raise an exception
  IF upcoming_appointments > 0 THEN
    RAISE EXCEPTION 'Cannot delete patient with upcoming appointments';
  END IF;

  -- Return the old record
  RETURN OLD;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER patient_deletion_trigger
BEFORE DELETE ON PATIENT
FOR EACH ROW
EXECUTE FUNCTION prevent_patient_deletion();


-- This function prevents the update of a doctor's department if the doctor has upcoming appointments
CREATE OR REPLACE FUNCTION prevent_department_update() RETURNS TRIGGER AS $$
DECLARE
  upcoming_appointments INT;
BEGIN
  -- Count the number of upcoming appointments for the doctor
  SELECT COUNT(*) INTO upcoming_appointments
  FROM APPOINTMENT
  WHERE doctor_id = NEW.doctor_id AND date >= CURRENT_DATE;

  -- If the doctor has upcoming appointments and the department is being changed, raise an exception
  IF upcoming_appointments > 0 AND NEW.department_name <> OLD.department_name THEN
    RAISE EXCEPTION 'Cannot update department of doctor with upcoming appointments';
  END IF;

  -- Return the new record
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER doctor_department_update_trigger
BEFORE UPDATE ON DOCTOR
FOR EACH ROW
EXECUTE FUNCTION prevent_department_update();

-- This function prevents the deletion of a doctor who has upcoming appointments
CREATE OR REPLACE FUNCTION prevent_doctor_deletion() RETURNS TRIGGER AS $$
DECLARE
  upcoming_appointments INT;
BEGIN
  -- Count the number of upcoming appointments for the doctor
  SELECT COUNT(*) INTO upcoming_appointments
  FROM APPOINTMENT
  WHERE doctor_id = OLD.doctor_id AND date >= CURRENT_DATE;

  -- If the doctor has upcoming appointments, raise an exception
  IF upcoming_appointments > 0 THEN
    RAISE EXCEPTION 'Cannot delete doctor with upcoming appointments';
  END IF;

  -- Return the old record
  RETURN OLD;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER doctor_deletion_trigger
BEFORE DELETE ON DOCTOR
FOR EACH ROW
EXECUTE FUNCTION prevent_doctor_deletion();

-- This function inserts a new surgeon into the SURGEON table
CREATE FUNCTION insert_surgeon()
RETURNS TRIGGER AS $$
BEGIN
    -- Insert the new doctor into the SURGEON table
    INSERT INTO SURGEON (doctor_id) VALUES (NEW.doctor_id);
    -- Return the new record
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_InsertSurgeon
AFTER INSERT ON DOCTOR
FOR EACH ROW
WHEN (NEW.specialization = 'surgeon')
EXECUTE FUNCTION insert_surgeon();

-- This function inserts a new general practitioner into the GENERAL_PRACTITIONER table
CREATE FUNCTION insert_general_practitioner()
RETURNS TRIGGER AS $$
BEGIN
    -- Insert the new doctor into the GENERAL_PRACTITIONER table
    INSERT INTO GENERAL_PRACTITIONER (doctor_id) VALUES (NEW.doctor_id);
    -- Return the new record
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_InsertGeneralPractitioner
AFTER INSERT ON DOCTOR
FOR EACH ROW
WHEN (NEW.specialization = 'general_practitioner')
EXECUTE FUNCTION insert_general_practitioner();

-- This function updates the specialization of a doctor in the SURGEON and GENERAL_PRACTITIONER tables
CREATE FUNCTION update_specialization()
RETURNS TRIGGER AS $$
BEGIN
    -- If the doctor's specialization was updated from "surgeon", delete the corresponding record from the SURGEON```sql
-- table
    IF OLD.specialization = 'surgeon' THEN
        DELETE FROM SURGEON WHERE doctor_id = OLD.doctor_id;
    END IF;

    -- If the doctor's specialization was updated to "surgeon", insert a new record into the SURGEON table
    IF NEW.specialization = 'surgeon' THEN
        INSERT INTO SURGEON (doctor_id) VALUES (NEW.doctor_id);
    END IF;

    -- If the doctor's specialization was updated from "general practitioner", delete the corresponding record from the GENERAL_PRACTITIONER table
    IF OLD.specialization = 'general_practitioner' THEN
        DELETE FROM GENERAL_PRACTITIONER WHERE doctor_id = OLD.doctor_id;
    END IF;

    -- If the doctor's specialization was updated to "general practitioner", insert a new record into the GENERAL_PRACTITIONER table
    IF NEW.specialization = 'general_practitioner' THEN
        INSERT INTO GENERAL_PRACTITIONER (doctor_id) VALUES (NEW.doctor_id);
    END IF;

    -- Return the new record
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_UpdateSpecialization
AFTER UPDATE ON DOCTOR
FOR EACH ROW
WHEN (OLD.specialization <> NEW.specialization)
EXECUTE FUNCTION update_specialization();

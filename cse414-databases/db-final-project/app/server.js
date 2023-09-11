const express = require("express");
const { Pool } = require("pg");

const pool = new Pool({
  user: "omer",
  host: "localhost",
  database: "omer",
  password: "123456",
  port: 5432,
});

const app = express();

app.use(express.json());

app.use((req, res, next) => {
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE");
  res.setHeader("Access-Control-Allow-Headers", "Content-Type");
  next();
});

// Get all patient_types
app.get("/patient_types", async (req, res) => {
  try {
    const result = await pool.query("SELECT * FROM PATIENT_TYPE");
    res.json(result.rows);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Get all patients
app.get("/patients", async (req, res) => {
  try {
    const result = await pool.query("SELECT * FROM PATIENT");
    res.json(result.rows);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Get a patient by id
app.get("/patients/:id", async (req, res) => {
  try {
    const result = await pool.query(
      "SELECT * FROM PATIENT WHERE patient_id = $1",
      [req.params.id]
    );
    res.json(result.rows[0]);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Add a new patient
app.post("/patients", async (req, res) => {
  try {
    const { name, address, date_of_birth, gender, phone_number, type } =
      req.body;
    const result = await pool.query(
      "INSERT INTO PATIENT (name, address, date_of_birth, gender, phone_number, type) VALUES ($1, $2, $3, $4, $5, $6) RETURNING *",
      [name, address, date_of_birth, gender, phone_number, type]
    );
    res.json(result.rows[0]);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Update a patient
app.put("/patients/:id", async (req, res) => {
  try {
    const { name, address, date_of_birth, gender, phone_number, type } =
      req.body;
    const result = await pool.query(
      "UPDATE PATIENT SET name = $1, address = $2, date_of_birth = $3, gender = $4, phone_number = $5, type = $6 WHERE patient_id = $7 RETURNING *",
      [name, address, date_of_birth, gender, phone_number, type, req.params.id]
    );
    res.json(result.rows[0]);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Delete a patient
app.delete("/patients/:id", async (req, res) => {
  try {
    const result = await pool.query(
      "DELETE FROM PATIENT WHERE patient_id = $1 RETURNING *",
      [req.params.id]
    );
    res.json(result.rows[0]);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Get all doctors
app.get("/doctors", async (req, res) => {
  try {
    const result = await pool.query("SELECT * FROM DOCTOR");
    res.json(result.rows);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Get all practitioners
app.get("/practitioners", async (req, res) => {
  try {
    const result = await pool.query(
      "SELECT * FROM DOCTOR d JOIN GENERAL_PRACTITIONER p ON d.doctor_id = p.doctor_id"
    );
    res.json(result.rows);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Get all surgeons
app.get("/surgeons", async (req, res) => {
  try {
    const result = await pool.query(
      "SELECT * FROM DOCTOR d JOIN SURGEON s ON d.doctor_id = s.doctor_id"
    );
    res.json(result.rows);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Get a doctor by id
app.get("/doctors/:id", async (req, res) => {
  try {
    const result = await pool.query(
      "SELECT * FROM DOCTOR WHERE doctor_id = $1",
      [req.params.id]
    );
    res.json(result.rows[0]);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Get doctors by department name
app.get("/doctors/department/:department_name", async (req, res) => {
  try {
    const result = await pool.query(
      "SELECT * FROM DOCTOR WHERE LOWER(department_name) = $1",
      [req.params.department_name.toLowerCase()]
    );
    res.json(result.rows);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Add a new doctor
app.post("/doctors", async (req, res) => {
  try {
    const {
      name,
      specialization,
      date_of_joining,
      phone_number,
      department_name,
    } = req.body;
    const result = await pool.query(
      "INSERT INTO DOCTOR (name, specialization, date_of_joining, phone_number, department_name) VALUES ($1, $2, $3, $4, $5) RETURNING *",
      [name, specialization, date_of_joining, phone_number, department_name]
    );
    res.json(result.rows[0]);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Update a doctor
app.put("/doctors/:id", async (req, res) => {
  try {
    const {
      name,
      specialization,
      date_of_joining,
      phone_number,
      department_name,
    } = req.body;
    const result = await pool.query(
      "UPDATE DOCTOR SET name = $1, specialization = $2, date_of_joining = $3, phone_number = $4, department_name = $5 WHERE doctor_id = $6 RETURNING *",
      [
        name,
        specialization,
        date_of_joining,
        phone_number,
        department_name,
        req.params.id,
      ]
    );
    res.json(result.rows[0]);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Delete a doctor
app.delete("/doctors/:id", async (req, res) => {
  try {
    const result = await pool.query(
      "DELETE FROM DOCTOR WHERE doctor_id = $1 RETURNING *",
      [req.params.id]
    );
    res.json(result.rows[0]);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Get all appointments
app.get("/appointments", async (req, res) => {
  try {
    const result = await pool.query(
      "SELECT a.appointment_id, a.date, a.time, a.purpose, p.name as patient_name, d.name as doctor_name, a.department_name FROM APPOINTMENT a JOIN PATIENT p ON a.patient_id=p.patient_id JOIN DOCTOR d ON a.doctor_id=d.doctor_id"
    );
    res.json(result.rows);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Get all doctor appointments
app.get("/doctor_appointments", async (req, res) => {
  try {
    const result = await pool.query("SELECT * FROM doctor_appointments");
    res.json(result.rows);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Get a appointment by id
app.get("/appointments/:id", async (req, res) => {
  try {
    const result = await pool.query(
      "SELECT * FROM APPOINTMENT WHERE appointment_id = $1",
      [req.params.id]
    );
    res.json(result.rows[0]);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Add a new appointment
app.post("/appointments", async (req, res) => {
  try {
    const { date, time, purpose, patient_id, department_name, doctor_id } =
      req.body;
    const result = await pool.query(
      "INSERT INTO APPOINTMENT (date, time, purpose, patient_id, department_name, doctor_id) VALUES ($1, $2, $3, $4, $5, $6) RETURNING *",
      [date, time, purpose, patient_id, department_name, doctor_id]
    );
    res.json(result.rows[0]);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Update a appointment
app.put("/appointments/:id", async (req, res) => {
  try {
    const { date, time, purpose, patient_id, department_name, doctor_id } =
      req.body;
    const result = await pool.query(
      "UPDATE APPOINTMENT SET date = $1, time = $2, purpose = $3, patient_id = $4, department_name = $5, doctor_id = $6 WHERE appointment_id = $7 RETURNING *",
      [
        date,
        time,
        purpose,
        patient_id,
        department_name,
        doctor_id,
        req.params.id,
      ]
    );
    res.json(result.rows[0]);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Delete a appointment
app.delete("/appointments/:id", async (req, res) => {
  try {
    const result = await pool.query(
      "DELETE FROM APPOINTMENT WHERE appointment_id = $1 RETURNING *",
      [req.params.id]
    );
    res.json(result.rows[0]);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

// Get all departments
app.get("/departments", async (req, res) => {
  try {
    const result = await pool.query("SELECT * FROM DEPARTMENT");
    res.json(result.rows);
  } catch (error) {
    console.error("Error:", error.message);
    res.status(500).json({ error: error.message });
  }
});

app.listen(3000, () => {
  console.log("Server is running on port 3000");
});

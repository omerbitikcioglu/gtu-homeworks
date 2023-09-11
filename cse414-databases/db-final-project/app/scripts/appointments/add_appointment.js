document.addEventListener("DOMContentLoaded", function () {
  // Get the department dropdown element
  const departmentDropdown = document.getElementById("department_name");

  // Populate patients menu
  axios
    .get("http://localhost:3000/patients")
    .then((response) => {
      const patients = response.data;
      const patientSelect = document.getElementById("patient_id");
      patients.forEach((patient) => {
        const option = document.createElement("option");
        option.text = patient.name;
        option.value = patient.patient_id;
        patientSelect.add(option);
      });
    })
    .catch((error) => {
      console.error("Error:", error);
    });

  // Populate departments menu
  axios
    .get("http://localhost:3000/departments")
    .then((response) => {
      const departments = response.data;
      departments.forEach((department) => {
        const option = document.createElement("option");
        option.text = department.name;
        option.value = department.name;
        departmentDropdown.add(option);
      });

      departmentDropdown.dispatchEvent(new Event("change"));
    })
    .catch((error) => {
      console.error("Error:", error);
    });

  // Attach the event listener
  departmentDropdown.addEventListener("change", function () {
    const selectedDepartment = departmentDropdown.value;

    // Make a request to fetch the doctors in the selected department
    axios
      .get(`http://localhost:3000/doctors/department/${selectedDepartment}`)
      .then((response) => {
        const doctorsDropdown = document.getElementById("doctor_id");

        // Clear existing options
        doctorsDropdown.innerHTML = "";

        // Add new options for the doctors
        response.data.forEach((doctor) => {
          const option = document.createElement("option");
          option.value = doctor.doctor_id;
          option.textContent = doctor.name;
          doctorsDropdown.appendChild(option);
        });
      })
      .catch((error) => {
        console.error("Error:", error);
      });
  });
});

// Submit
document
  .getElementById("add-appointment-form")
  .addEventListener("submit", (event) => {
    event.preventDefault();

    // Validate the form
    if (!validateForm()) {
      return; // Stop further execution if the form validation fails
    }

    const date = moment(document.getElementById("date").value)
      .locale("tr")
      .format("YYYY-MM-DD");
    const time = document.getElementById("time").value;
    const purpose = document.getElementById("purpose").value;
    const patient_id = document.getElementById("patient_id").value;
    const department_name = document.getElementById("department_name").value;
    const doctor_id = document.getElementById("doctor_id").value;

    const appointment = {
      date,
      time,
      purpose,
      patient_id,
      department_name,
      doctor_id,
    };

    axios
      .post("http://localhost:3000/appointments", appointment)
      .then((response) => {
        alert("Appointment added successfully.");
        window.location.href = "appointments.html";
      })
      .catch((error) => {
        console.error("Error:", error);
        if (error.response) {
          alert(error.response.data.error);
        } else {
          alert("An error occurred while adding the appointment.");
        }
      });
  });

function validateForm() {
  // Get the date input value
  const dateInput = document.getElementById("date");
  const selectedDate = moment(dateInput.value, "YYYY-MM-DD");

  // Get the current date
  const currentDate = moment();

  // Compare the selected date with the current date
  if (selectedDate.isSameOrBefore(currentDate)) {
    alert("Please select a future date for the appointment.");
    return false; // Return false if the form validation fails
  }

  return true; // Return true if the form validation passes
}

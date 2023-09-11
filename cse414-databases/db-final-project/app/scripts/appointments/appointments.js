document.addEventListener("DOMContentLoaded", () => {
  loadAppointments();
  loadDoctorAppointments();
});

function loadAppointments() {
  axios
    .get("http://localhost:3000/appointments")
    .then((response) => {
      const appointments = response.data;
      const appointmentsTable = document.getElementById("appointments-body");
      appointmentsTable.innerHTML = "";
      appointments.forEach((appointment) => {
        const row = appointmentsTable.insertRow();
        row.innerHTML = `<td>${appointment.appointment_id}</td>
                         <td>${moment(appointment.date).format(
                           "DD/MM/YYYY"
                         )}</td>
                         <td>${moment(appointment.time, "HH:mm:ss").format(
                           "HH:mm"
                         )}</td>
                         <td>${appointment.purpose}</td>
                         <td>${appointment.patient_name}</td>
                         <td>${appointment.doctor_name}</td>
                         <td>${appointment.department_name}</td>
                         <td>
                            <button onclick="editAppointment(${
                              appointment.appointment_id
                            })" class="btn btn-primary">Edit</button>
                            <button onclick="deleteAppointment(${
                              appointment.appointment_id
                            })" class="btn btn-danger">Delete</button>
                         </td>`;
      });
    })
    .catch((error) => {
      console.error("Error:", error);
    });
}

function loadDoctorAppointments() {
  axios
    .get("http://localhost:3000/doctor_appointments")
    .then((response) => {
      const doctor_appointments = response.data;
      const appointmentsViewTable = document.getElementById(
        "appointments-view-body"
      );
      appointmentsViewTable.innerHTML = "";
      doctor_appointments.forEach((doctor_appointment) => {
        const row = appointmentsViewTable.insertRow();
        row.innerHTML = `<td>${doctor_appointment.doctor_id}</td>
                         <td>${doctor_appointment.name}</td>
                         <td>${moment(doctor_appointment.date).format(
                           "DD/MM/YYYY"
                         )}</td>
                        <td>${moment(
                          doctor_appointment.time,
                          "HH:mm:ss"
                        ).format("HH:mm")}</td>`;
      });
    })
    .catch((error) => {
      console.error("Error:", error);
    });
}

function editAppointment(appointmentId) {
  window.location.href = `edit_appointment.html?id=${appointmentId}`;
}

function deleteAppointment(appointmentId) {
  axios
    .delete(`http://localhost:3000/appointments/${appointmentId}`)
    .then((response) => {
      alert("Appointment deleted successfully.");
      loadAppointments();
      loadDoctorAppointments();
    })
    .catch((error) => {
      console.error("Error:", error);
      if (error.response) {
        alert(error.response.data.error);
      } else {
        alert("An error occurred while deleting the appointment.");
      }
    });
}

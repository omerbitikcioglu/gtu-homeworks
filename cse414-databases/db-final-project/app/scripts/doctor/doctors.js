document.addEventListener("DOMContentLoaded", () => {
  loadDoctors();
  loadPractitioners();
  loadSurgeons();
});

function loadDoctors() {
  axios
    .get("http://localhost:3000/doctors")
    .then((response) => {
      const doctors = response.data;
      const doctorsTable = document.getElementById("doctors-body");
      const practitionersTable = document.getElementById("practitioners-body");
      const surgeonsTable = document.getElementById("surgeons-body");
      doctorsTable.innerHTML = "";
      doctors.forEach((doctor) => {
        const row = doctorsTable.insertRow();
        row.innerHTML = `<td>${doctor.doctor_id}</td>
                                 <td>${doctor.name}</td>
                                 <td>${doctor.specialization}</td>
                                 <td>${moment(doctor.date_of_joining).format(
                                   "DD/MM/YYYY"
                                 )}</td>
                                 <td>${doctor.phone_number}</td>
                                 <td>${doctor.department_name}</td>
                                 <td>
                                    <button onclick="editDoctor(${
                                      doctor.doctor_id
                                    })" class="btn btn-primary">Edit</button>
                                    <button onclick="deleteDoctor(${
                                      doctor.doctor_id
                                    })" class="btn btn-danger">Delete</button>
                                 </td>`;
      });
    })
    .catch((error) => {
      console.error("Error:", error);
    });
}

function loadPractitioners() {
  axios
    .get("http://localhost:3000/practitioners")
    .then((response) => {
      const practitioners = response.data;
      const practitionersTable = document.getElementById("practitioners-body");
      practitionersTable.innerHTML = "";
      practitioners.forEach((practitioner) => {
        const row = practitionersTable.insertRow();
        row.innerHTML = `<td>${practitioner.doctor_id}</td>
                                 <td>${practitioner.name}</td>
                                 <td>${practitioner.specialization}</td>
                                 <td>${moment(
                                   practitioner.date_of_joining
                                 ).format("DD/MM/YYYY")}</td>
                                 <td>${practitioner.phone_number}</td>
                                 <td>${practitioner.department_name}</td>`;
      });
    })
    .catch((error) => {
      console.error("Error:", error);
    });
}

function loadSurgeons() {
  axios
    .get("http://localhost:3000/surgeons")
    .then((response) => {
      const surgeons = response.data;
      const surgeonsTable = document.getElementById("surgeons-body");
      surgeonsTable.innerHTML = "";
      surgeons.forEach((surgeon) => {
        const row = surgeonsTable.insertRow();
        row.innerHTML = `<td>${surgeon.doctor_id}</td>
                                 <td>${surgeon.name}</td>
                                 <td>${surgeon.specialization}</td>
                                 <td>${moment(surgeon.date_of_joining).format(
                                   "DD/MM/YYYY"
                                 )}</td>
                                 <td>${surgeon.phone_number}</td>
                                 <td>${surgeon.department_name}</td>`;
      });
    })
    .catch((error) => {
      console.error("Error:", error);
    });
}

function editDoctor(doctorId) {
  window.location.href = `edit_doctor.html?id=${doctorId}`;
}

function deleteDoctor(doctorId) {
  axios
    .delete(`http://localhost:3000/doctors/${doctorId}`)
    .then((response) => {
      alert("Doctor deleted successfully.");
      loadDoctors();
      loadPractitioners();
      loadSurgeons();
    })
    .catch((error) => {
      console.error("Error:", error);
      if (error.response) {
        alert(error.response.data.error);
      } else {
        alert("An error occurred while deleting the doctor.");
      }
    });
}

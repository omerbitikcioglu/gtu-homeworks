document.addEventListener("DOMContentLoaded", () => {
  loadPatients();
});

function loadPatients() {
  axios
    .get("http://localhost:3000/patients")
    .then((response) => {
      const patients = response.data;
      const patientsTable = document.getElementById("patients-body");
      patientsTable.innerHTML = "";
      patients.forEach((patient) => {
        const row = patientsTable.insertRow();
        row.innerHTML = `<td>${patient.patient_id}</td>
                                 <td>${patient.name}</td>
                                 <td>${patient.address}</td>
                                 <td>${moment(patient.date_of_birth).format(
                                   "DD/MM/YYYY"
                                 )}</td>
                                 <td>${patient.gender}</td>
                                 <td>${patient.phone_number}</td>
                                 <td>${patient.type}</td>
                                 <td>
                                    <button onclick="editPatient(${
                                      patient.patient_id
                                    })" class="btn btn-primary">Edit</button>
                                    <button onclick="deletePatient(${
                                      patient.patient_id
                                    })" class="btn btn-danger">Delete</button>
                                 </td>`;
      });
    })
    .catch((error) => {
      console.error("Error:", error);
    });
}

function editPatient(patientId) {
  window.location.href = `edit_patient.html?id=${patientId}`;
}

function deletePatient(patientId) {
  axios
    .delete(`http://localhost:3000/patients/${patientId}`)
    .then((response) => {
      alert("Patient deleted successfully.");
      loadPatients();
    })
    .catch((error) => {
      console.error("Error:", error);
      if (error.response) {
        alert(error.response.data.error);
      } else {
        alert("An error occurred while deleting the patient.");
      }
    });
}

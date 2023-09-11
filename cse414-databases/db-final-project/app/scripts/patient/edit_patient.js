const urlParams = new URLSearchParams(window.location.search);
const patientId = urlParams.get("id");

axios
  .get("http://localhost:3000/patient_types")
  .then((response) => {
    const patient_types = response.data;
    const patientTypeSelect = document.getElementById("patient_type");
    patient_types.forEach((patientType) => {
      const option = document.createElement("option");
      option.text = patientType.type;
      option.value = patientType.type;
      patientTypeSelect.add(option);
    });
  })
  .catch((error) => {
    console.error("Error:", error);
    if (error.response) {
      alert(error.response.data.error);
    }
  });

axios
  .get(`http://localhost:3000/patients/${patientId}`)
  .then((response) => {
    const patient = response.data;

    document.getElementById("name").value = patient.name;
    document.getElementById("address").value = patient.address;
    const dateOfBirth = moment(patient.date_of_birth).format("YYYY-MM-DD");
    document.getElementById("date_of_birth").value = dateOfBirth;
    document.getElementById("gender").value = patient.gender;
    document.getElementById("phone_number").value = patient.phone_number;
    document.getElementById("patient_type").value = patient.type;
  })
  .catch((error) => {
    console.error("Error:", error);
  });

document
  .getElementById("edit-patient-form")
  .addEventListener("submit", (event) => {
    event.preventDefault();
    const name = document.getElementById("name").value;
    const address = document.getElementById("address").value;
    const dateOfBirth = moment(document.getElementById("date_of_birth").value)
      .locale("tr")
      .format("YYYY-MM-DD");
    const gender = document.getElementById("gender").value;
    const phone_number = document.getElementById("phone_number").value;
    const type = document.getElementById("patient_type").value;

    const patient = {
      name,
      address,
      date_of_birth: dateOfBirth,
      gender,
      phone_number,
      type,
    };

    axios
      .put(`http://localhost:3000/patients/${patientId}`, patient)
      .then((response) => {
        alert("Patient updated successfully.");
        window.location.href = "patients.html";
      })
      .catch((error) => {
        console.error("Error:", error);
        if (error.response) {
          alert(error.response.data.error);
        } else {
          alert("An error occurred while editing the patient.");
        }
      });
  });

const urlParams = new URLSearchParams(window.location.search);
const doctorId = urlParams.get("id");

axios
  .get("http://localhost:3000/departments")
  .then((response) => {
    const departments = response.data;
    const departmentSelect = document.getElementById("department_name");
    departments.forEach((department) => {
      const option = document.createElement("option");
      option.text = department.name;
      option.value = department.name;
      departmentSelect.add(option);
    });
  })
  .catch((error) => {
    console.error("Error:", error);
  });

axios
  .get(`http://localhost:3000/doctors/${doctorId}`)
  .then((response) => {
    const doctor = response.data;

    document.getElementById("name").value = doctor.name;
    document.getElementById("specialization").value = doctor.specialization;
    const dateOfJoin = moment(doctor.date_of_joining).format("YYYY-MM-DD");
    document.getElementById("date_of_joining").value = dateOfJoin;
    document.getElementById("phone_number").value = doctor.phone_number;
    document.getElementById("department_name").value = doctor.department_name;
  })
  .catch((error) => {
    console.error("Error:", error);
  });

document
  .getElementById("edit-doctor-form")
  .addEventListener("submit", (event) => {
    event.preventDefault();
    const name = document.getElementById("name").value;
    const specialization = document.getElementById("specialization").value;
    const dateOfJoin = moment(document.getElementById("date_of_joining").value)
      .locale("tr")
      .format("YYYY-MM-DD");
    const phone_number = document.getElementById("phone_number").value;
    const department_name = document.getElementById("department_name").value;

    const doctor = {
      name,
      specialization,
      date_of_joining: dateOfJoin,
      phone_number,
      department_name,
    };

    axios
      .put(`http://localhost:3000/doctors/${doctorId}`, doctor)
      .then((response) => {
        alert("Doctor updated successfully.");
        window.location.href = "doctors.html";
      })
      .catch((error) => {
        console.error("Error:", error);
        if (error.response) {
          alert(error.response.data.error);
        } else {
          alert("An error occurred while editing the doctor.");
        }
      });
  });

document
  .getElementById("add-doctor-form")
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
      .post("http://localhost:3000/doctors", doctor)
      .then((response) => {
        alert("Doctor added successfully.");
        window.location.href = "doctors.html";
      })
      .catch((error) => {
        console.error("Error:", error);
      });
  });

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
    if (error.response) {
      alert(error.response.data.error);
    } else {
      alert("An error occurred while adding the doctor.");
    }
  });

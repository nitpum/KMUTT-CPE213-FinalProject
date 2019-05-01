const header = ["age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target"]

header.forEach(function (item) {
  console.log(`cleveland[cleveland\$${item} == "?"] = NULL`)
})
var express = require('express');
var app     = express();

app.use(express.static(__dirname)); 

app.listen(8000);
console.log("Bedrock test servlet running on port 8000.");
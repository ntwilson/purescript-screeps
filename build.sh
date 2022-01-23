spago bundle-app 

echo "module.exports.loop = function() {" > output.js
cat index.js >> output.js 
echo "}" >> output.js

cat output.js | clip.exe

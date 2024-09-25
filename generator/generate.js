const dataSource = require("zigbee-herdsman-converters");
const fs = require("node:fs");
const path = require("node:path");

for (const definition of dataSource.definitions) {
  const model = definition.model.replace(/[^a-zA-Z0-9]/g, "_");
  const definitionPath = `../definitions/${definition.vendor}/${model}.json`;
  fs.mkdirSync(path.dirname(definitionPath), { recursive: true });
  fs.writeFileSync(definitionPath, JSON.stringify(definition, null, 4));
}

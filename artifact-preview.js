#!/usr/bin/env node

// https://github.com/themadcreator/circle-github-bot#readme

const bot = require("circle-github-bot").create();

bot.comment(`
Preview for <i>${bot.env.commitMessage}</i>: <strong>${bot.artifactLink('generated-site/index.html', 'index.html')}</strong>
`);

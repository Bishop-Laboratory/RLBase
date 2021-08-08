# Client side README

## Requirements

You need Node.js to be able to run the project. You can download from the official site:

```shell
https://nodejs.org/en/
```

Verify that Node and npm (included in Node) were installed:

```shell
node -v
npm -v
```

## Set up and run the front-end app in a dev environment

To set up the client side, clone this repo:

```shell
git clone https://github.com/Bishop-Laboratory/RMapDB-v2.git
```

Then, change to the repo folder and install dependencies:

```shell
cd RMapDB-v2/client/
npm install
```

Launch the React app (currently is alpha version):

```shell
npm start
```

## Linting

ESLint and Prettier are used for code formatting and quality in this project.

To lint and automatically fix stylistic errors in the code, run:

```shell
npm run lint
```

If you're coding on VS Code, you can also configure auto-fix for your files when you save your code. See [here](https://javascript.plainenglish.io/setting-eslint-and-prettier-on-a-react-typescript-project-2021-22993565edf9) for more info about how ESLint + Prettier was set up, as well as how to configure auto-formatting in VS Code.

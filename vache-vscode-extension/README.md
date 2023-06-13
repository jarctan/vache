Vache syntax highlighting for VSCode

# Contribution setup

Run `npm i` at the beginning to install dependencies.

Then `npm run watch` to reverberate any changes to the `.yml` file into the `.json` file. Do **NOT**
modify the `.json` file, only the `.yml` one. To run the watcher you can also run the corresponding
VSCode Task.

Do your things.

Use the `Run and Debug` VSCode panel to launch the extension in a specific window.

Once the extension is ready, do `npm run package` to get your `vache-[...].vsix` that you can install!
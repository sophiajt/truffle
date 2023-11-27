// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import {
	LanguageClient,
	LanguageClientOptions,
    ServerOptions
} from "vscode-languageclient/node";

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "truffle" is now active!');

	// The command has been defined in the package.json file
	// Now provide the implementation of the command with registerCommand
	// The commandId parameter must match the command field in package.json
	// let disposable = vscode.commands.registerCommand('truffle.helloWorld', () => {
	// 	// The code you place here will be executed every time your command is executed
	// 	// Display a message box to the user
	// 	vscode.window.showInformationMessage('Hello World from truffle!');
	// });

	const serverOptions: ServerOptions = {
		command: 'truffle-lsp', // Replace with your own command.
		args: [],
	};
	  
	const clientOptions: LanguageClientOptions = {
		documentSelector: [
			// Active functionality on files of these languages.
			{
				language: 'truffle',
			},
		],
	};
	  
	const client = new LanguageClient('truffle-language-server', serverOptions, clientOptions);
	client.start();
	
	// context.subscriptions.push(disposable);
}

// This method is called when your extension is deactivated
export function deactivate() {}

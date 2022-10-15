enum TokenType {
	Lambda,
	Ident,
	Sep,
	LeftParen,
	RightParen,
	Equal,
	End
}

interface IRawToken {
	line: number;
	startCharacter: number;
	content: string;
	tokenType: TokenType;
}

interface IAnalyzedToken {
	line: number;
	startCharacter: number;
	length: number;
	tokenType: string;
}

export class LamdbaParser {
	private tokenize(text: string): IRawToken[] {
		let tokens: IRawToken[] = [];
		const lines = text.split(/\r\n|\r|\n/);

		for (const [lineCount, lineText] of lines.entries()) {
			let offset = 0;
			function pushToken(textLength: number, tokenType: TokenType) {
				tokens.push({
					line: lineCount,
					startCharacter: offset,
					content: lineText.substring(offset, offset + textLength),
					tokenType: tokenType
				});
				offset += textLength;
			}

			while (offset < lineText.length) {
				if (lineText.startsWith("lambda", offset)) {
					pushToken(6, TokenType.Lambda);
				}
				else if (lineText.startsWith("(", offset)) {
					pushToken(1, TokenType.LeftParen);
				}
				else if (lineText.startsWith(")", offset)) {
					pushToken(1, TokenType.RightParen);
				}
				else if (lineText.startsWith(".", offset)) {
					pushToken(1, TokenType.Sep);
				}
				else if (lineText.startsWith("=", offset)) {
					pushToken(1, TokenType.Equal);
				}
				else {
					const match = lineText.substring(offset).match(/^[a-zA-Z][a-zA-Z0-9]*/);
					if (match) {
						pushToken(match[0].length, TokenType.Ident);
					}
					else {
						const match = lineText.substring(offset).match(/^\s+/);
						if (match) {
							offset += match[0].length;
						}
						else break;
					}
				}
			}
			pushToken(0, TokenType.End);
		}
		return tokens;
	}

	public parse(text: string): IAnalyzedToken[] {
		let tokens = this.tokenize(text);
		tokens = tokens.reverse();
		let analyzedTokens: IAnalyzedToken[] = [];
		let context: (string | undefined)[] = [];
		while (tokens.length > 0) {
			const token = tokens.pop();
			switch (token?.tokenType) {
				case TokenType.Lambda:
					const nextToken = tokens.pop();
					if (nextToken?.tokenType == TokenType.Ident) {
						analyzedTokens.push({
							line: nextToken.line,
							startCharacter: nextToken.startCharacter,
							length: nextToken.content.length,
							tokenType: "parameter"
						});
						context.push(nextToken.content);
					}
					break;
				case TokenType.Ident:
					if (context.includes(token.content)) {
						analyzedTokens.push({
							line: token.line,
							startCharacter: token.startCharacter,
							length: token.content.length,
							tokenType: "parameter"
						});
					}
					else {
						analyzedTokens.push({
							line: token.line,
							startCharacter: token.startCharacter,
							length: token.content.length,
							tokenType: "function"
						});
					}
					break;
				case TokenType.LeftParen:
					context.push(undefined);
					break;
				case TokenType.RightParen:
					while (context.pop());
					break;
			}
		}
		return analyzedTokens;
	}
}

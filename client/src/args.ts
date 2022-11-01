import { parse } from 'ts-command-line-args';
import { EOL } from 'os';

interface ISchemeIndexArgs {
    url?: string,
    filterset?: string,
    query?: string,
    rows?: number,
    page?: number,
    strict?: boolean,
    help?: boolean
}

const args = parse<ISchemeIndexArgs>({
    url: {
        type: String,
        alias: 'u',
        optional: true,
        description: 'Server url to make requests to. Defaults to https://index.scheme.org'
    },
    filterset: {
        type: String,
        alias: 'f',
        optional: true,
        description: 'Filterset (scheme implementation) to use (required for non-interactive mode)'
    },
    query: {
        type: String,
        alias: 'q',
        optional: true,
        description: 'Search query (required for non-interactive mode)'
    },
    rows: {
        type: Number,
        alias: 'r',
        optional: true,
        description: 'Row count / page size, defaults to 10'
    },
    page: {
        type: Number,
        alias: 'p',
        optional: true,
        description: 'Page index, 0-based (non-interactive mode)'
    },
    strict: {
        type: Boolean,
        alias: 's',
        optional: true,
        description: 'Use strict / exact search by name (non-interactive mode)'
    },
    help: { 
        type: Boolean, 
        optional: true, 
        alias: 'h', 
        description: 'Prints this usage guide'
    }
}, {
    helpArg: 'help',
    footerContentSections: [{
        content: `
                Scheme Index rest client${EOL}Report issues at https://github.com/arvyy/scheme-index-client${EOL}${EOL} 
                Client has 2 distinct modes -- interactive and non-interactive.${EOL} 
                Non-interactive mode accepts a query input through the argument, executes it, prints the result, and exits. It is  
                intended to be used programmatically from other software (for example, from VIM).  
                To start non-interactive mode, both q/query and f/filterset parameters must be provided.${EOL} 
                Interactive mode starts a REPL mode, accepting and printing queries until SIGINT is received.  ${EOL}${EOL} 
                Query is parsed as following. First, the line is tokenized by space. Each word that starts with 'p:' is interpreted as a parameter type filter.  
                Each word that starts with 'r:' is interpreted as a return type filter. Rest of the words are joined by space and passed as a query. For example,  
                'p:list? ref' would find functions that take a list as a parameter and have 'ref' in their name, such as the function list-ref. If running in interactive  
                mode, running just 'p:' or 'r:' will return the full list of possible parameter or return types, respectively. 
                In interactive mode upon receiving paginated result, enter a query consisting of just an integer to show appropriate page (1 - based).
                `
    }]
});

args.url = args.url || 'https://index.scheme.org';
args.rows = args.rows || 10;
args.page = args.page || 0;
args.strict = args.strict || false;

export { args };

import { Component, Input } from '@angular/core';
import { Params } from '@angular/router';
import { ReplaySubject, Observable, map } from 'rxjs';
import { SearchItem, Signature } from '../model';

@Component({
    selector: 'app-search-item',
    templateUrl: './search-item.component.html',
    styleUrls: ['./search-item.component.scss']
})
export class SearchItemComponent {

    searchitem$ = new ReplaySubject<SearchItem>(1);
    componentSearchItem: Observable<ComponentSearchItem>;

    constructor() {
        this.componentSearchItem = this.searchitem$.pipe(map(s => {
            const res: ComponentSearchItem = {
                name: s.name,
                description: s.description,
                searchItem: s,
                lib: {
                    link: this.routerResolver(s, 'lib', s.lib),
                    label: s.lib
                },
                tags: s.tags.map(t => {
                    return {
                        label: t,
                        link: this.routerResolver(s, 'tag', t)
                    };
                }),
                signature: s.signature,
                subsignatures: s.subsignatures
            };
            return res;
        }));
    }

    @Input()
    set searchitem(item: SearchItem) {
        this.searchitem$.next(item);
    }

    @Input()
    routerResolver!: RouterLinkResolver;

    isAuxiliaryType(type: string) {
        switch (type) {
            case '...':
            case '#f':
            case '*':
                return true;
            default:
                return false;
        }
    }

    highlightSyntaxSignature(literals: string[], pattern: string): TextPart[] {
        const firstSpace = pattern.indexOf(' ');
        return [...this.highlightLiterals(literals, pattern.substring(firstSpace))];
    }

    highlightLiterals(literals: string[], pattern: string): TextPart[] {
        const parts: TextPart[] = [];
        let index = 0;
        for (;;) {
            let nextLiteralIndex = -1;
            let nextLiteral = '';
            for (const l of literals) {
                const i = pattern.indexOf(l, index);
                if (i == -1)
                    continue;
                //do not highlight if it's inside another identifier
                const prec = pattern.charAt(i - 1);
                const follow = pattern.charAt(i + l.length);
                if (prec && prec.match(/[^ ()]/))
                    continue;
                if (follow && follow.match(/[^ ()]/))
                    continue;
                if (nextLiteralIndex == -1 || i < nextLiteralIndex) {
                    nextLiteralIndex = i;
                    nextLiteral = l;
                }
            }
            if (nextLiteralIndex == -1) {
                parts.push({
                    kind: 'plain',
                    text: pattern.substring(index, pattern.length)
                });
                break;
            }
            parts.push({
                kind: 'plain',
                text: pattern.substring(index, nextLiteralIndex)
            });
            parts.push({
                kind: 'literal',
                text: pattern.substring(nextLiteralIndex, nextLiteralIndex + nextLiteral.length)
            });
            index = nextLiteralIndex + nextLiteral.length;
        }
        return parts;
    }

    //hack to make typescript compiler shut up in template
    getSyntaxLiterals(signature: Signature): string[] {
        if (signature.type == 'syntax')
            return signature.literals;
        return [];
    }
}

interface Lib {
    label: string;
    link: RouterLink | null;
}

interface Tag {
    label: string;
    link: RouterLink | null;
}

interface ComponentSearchItem {
    searchItem: SearchItem,
    name: string;
    description: string;
    lib: Lib;
    tags: Tag[];
    signature: Signature,
    subsignatures: {
        name: string;
        signature: Signature;
    }[];
}

interface TextPart {
    kind: 'plain' | 'literal' | 'name';
    text: string;
}

export type RouterLink = { routerLink: string[]; queryParams: Params };
export type RouterLinkResolver = (item: SearchItem, type: 'param' | 'return' | 'tag' | 'name' | 'lib', value: string) => RouterLink | null

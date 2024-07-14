import { CommonModule } from '@angular/common';
import { Component, Input } from '@angular/core';
import { Params, RouterModule } from '@angular/router';
import { ReplaySubject, Observable, map } from 'rxjs';
import { SearchItem, SearchItemSingle, Signature } from '../index.types';

@Component({
    standalone: true,
    imports: [
        CommonModule,
        RouterModule
    ],
    selector: 'app-search-item',
    templateUrl: './search-item.component.html',
    styleUrls: ['./search-item.component.scss']
})
export class SearchItemComponent {

    searchitem$ = new ReplaySubject<SearchItem>(1);
    componentSearchItemGroup: Observable<ComponentSearchItemGroup>;

    constructor() {
        this.componentSearchItemGroup = this.searchitem$.pipe(map(s => {
            let entries: ComponentSearchItem[];
            let groupDesc = '';
            if (s.kind == 'single') {
                entries = [this.searchItemSingleToComponentSearchItem(s)];
            } else {
                entries = s.entries.map(e => this.searchItemSingleToComponentSearchItem(e));
                groupDesc = s.description;
            }
            return {
                lib: {
                    link: this.routerResolver(s, 'lib', s.lib),
                    label: s.lib
                },
                description: groupDesc,
                entries: entries
            };
        }));
    }

    @Input()
    set searchitem(item: SearchItem) {
        this.searchitem$.next(item);
    }

    @Input()
    routerResolver!: RouterLinkResolver;

    searchItemSingleToComponentSearchItem(s: SearchItemSingle) {
        return {
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
    }

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
        // name is already rendered in template and needs to be removed before rendering rest of signature
        const ommitedPrefix = pattern.match(/(^\([^\s)]*)(?:(?:\s)|(?:\)$))/);
        const ommitedPrefixEnd = ommitedPrefix? ommitedPrefix[1].length : 0;
        return [...this.highlightLiterals(literals, pattern.substring(ommitedPrefixEnd))];
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

interface ComponentSearchItemGroup {
    lib: Lib;
    description: string;
    entries: ComponentSearchItem[];
}

interface TextPart {
    kind: 'plain' | 'literal' | 'name';
    text: string;
}

export type RouterLink = { routerLink: string[]; queryParams: Params };
export type RouterLinkResolver = (item: SearchItem, type: 'param' | 'return' | 'tag' | 'name' | 'lib', value: string) => RouterLink | null

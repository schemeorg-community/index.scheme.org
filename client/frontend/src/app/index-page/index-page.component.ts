import { Component } from '@angular/core';
import { Observable, map } from 'rxjs';
import { IndexService } from '../index.service';
import { Download } from 'scmindex-common';
import { RouterModule } from '@angular/router';
import { CommonModule } from '@angular/common';

@Component({
    standalone: true,
    imports: [
        CommonModule,
        RouterModule
    ],
    selector: 'app-index-page',
    templateUrl: './index-page.component.html',
    styleUrls: ['./index-page.component.scss']
})
export class IndexPageComponent {

    filtersetGroups: Observable<FiltersetGroup[]>;
    downloads: Observable<Download[]>;

    constructor(filtersetsService: IndexService) {
        const isRnrs = (code: string) => {
            return code.match(/r.rs/);
        };
        this.filtersetGroups = filtersetsService.filtersets.pipe(map(filtersets => {
            return [{
                filtersets: filtersets.filter(f => isRnrs(f.code))
            }, {
                filtersets: filtersets.filter(f => !isRnrs(f.code))
            }].filter(e => e.filtersets.length);
        }));
        this.downloads = filtersetsService.downloads;
    }

    exampleVimConfig = `let g:scmindexFilterset="r7rs_small"
fu! ScmIndexGet()
    let wordUnderCursor = expand("<cword>")
    let result = system("scmindex -f " . g:scmindexFilterset . " -q " . wordUnderCursor . " -s")
    echo result
endfunc
nnoremap <silent> <leader>? :call ScmIndexGet()<cr>`;

    exampleUrl = "curl 'https://index.scheme.org/rest/filterset/r7rs_small/search?query=assoc&facet=false&rows=1'";

    exampleResponse = `{"total":4,"libs":[],"params":[],"returns":[],"tags":[],"items":[{"kind":"single","lib":"(scheme base)","name":"assoc","description":"It is an error if alist (for \"association list\") is not a list of pairs. This procedure finds the first pair in alist whose car field is obj, and returns that pair. If no pair in alist has obj as its car, then #f (not the empty list) is returned. The assoc procedure uses compare if given and equal? otherwise. Rationale: Although they are often used as predicates, memq, memv, member, assq, assv, and assoc do not have question marks in their names because they return potentially useful values rather than just #t or #f.","signature":{"type":"function","variants":[{"params":[{"name":"obj","types":[]},{"name":"alist","types":["list?"]}],"return":{"kind":"or","items":[{"kind":"return","type":"pair?"},{"kind":"return","type":"#f"}]}},{"params":[{"name":"obj","types":[]},{"name":"alist","types":["list?"]},{"name":"=","types":["procedure?"]}],"return":{"kind":"or","items":[{"kind":"return","type":"pair?"},{"kind":"return","type":"#f"}]}}]},"subsignatures":[{"name":"=","signature":{"type":"function","variants":[{"params":[{"name":"a","types":[]},{"name":"b","types":[]}],"return":{"kind":"return","type":"*"}}]}}],"tags":["pure"]}]}`;

}

interface FiltersetGroup {
    filtersets: Filterset[];
}

interface Filterset {
    code: string;
    name: string;
}

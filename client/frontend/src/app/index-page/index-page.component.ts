import { Component } from '@angular/core';
import { Observable, map } from 'rxjs';
import { IndexService } from '../index.service';
import { Download } from 'scmindex-common';

@Component({
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
            }];
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

    exampleUrl = "curl 'https://index.scheme.org/rest/filterset/chibi/search?query=assoc&facet=false&rows=1'";

    exampleResponse = `{"total":10,"libs":[],"params":[],"returns":[],"tags":[],"items":[{"lib":"(scheme list)","name":"assoc","description":"assoc, assq, assv alist must be an association list -- a list of pairs. These procedures find the first pair in alist whose car field is key, and returns that pair. If no pair in alist has key as its car, then #f is returned. assq uses eq? to compare key with the car fields of the pairs in alist, while assv uses eqv? and assoc uses equal?.  assoc is extended from its R5RS definition to allow the client to pass in an optional equality procedure = used to compare keys. The comparison procedure is used to compare the elements ei of list to the key parameter in this way: (= key (car ei)) ; list is (E1 ... En). That is, the first argument is always key, and the second argument is one of the list elements. Thus one can reliably find the first entry of alist whose key is greater than five with (assoc 5 alist <). Note that fully general alist searching may be performed with the find-tail and find procedures","signature":{"type":"function","variants":[{"params":[{"name":"obj","types":[]},{"name":"alist","types":["list?"]}],"return":{"kind":"or","items":[{"kind":"return","type":"list?"},{"kind":"return","type":"#f"}]}},{"params":[{"name":"obj","types":[]},{"name":"alist","types":["list?"]},{"name":"=","types":["procedure?"]}],"return":{"kind":"or","items":[{"kind":"return","type":"pair?"},{"kind":"return","type":"#f"}]}}]},"subsignatures":[{"name":"=","signature":{"type":"function","variants":[{"params":[{"name":"a","types":[]},{"name":"b","types":[]}],"return":{"kind":"return","type":"*"}}]}}],"tags":["pure"]}]}`;

}

interface FiltersetGroup {
    filtersets: Filterset[];
}

interface Filterset {
    code: string;
    name: string;
}

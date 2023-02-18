import { Subject, ReplaySubject, combineLatest, first } from 'rxjs';
import { Component, Input, Output, EventEmitter } from '@angular/core';
import { IndexQuery, IndexResponse, ResponseFacetValue } from '../model';
import { faMagnifyingGlass, faFolderOpen, faFolderClosed, faCircleChevronLeft, faCircleChevronRight } from '@fortawesome/free-solid-svg-icons';
import { FiltersetsService } from '../filtersets-service.service';

@Component({
  selector: 'app-filter-pane',
  templateUrl: './filter-pane.component.html',
  styleUrls: ['./filter-pane.component.scss']
})
export class FilterPaneComponent {

  @Input()
  collapsed = false;

  @Input()
  set query(value: IndexQuery) {
    this.query$.next(value);
  }

  @Input()
  set response(value: IndexResponse) {
    this.response$.next(value);
  }

  @Output()
  collapseChange = new EventEmitter<boolean>();

  @Output()
  queryChange = new EventEmitter<IndexQuery>();

  faSearch = faMagnifyingGlass;
  faFolderOpen = faFolderOpen;
  faFolderClosed = faFolderClosed;
  faCircleLeft = faCircleChevronLeft;
  faCircleRight = faCircleChevronRight;

  query$: Subject<IndexQuery>;
  response$: Subject<IndexResponse>;
  filterset = '';
  facets: Facet[] = [];
  queryString = '';

  constructor(private filtersetSvc: FiltersetsService) {
    this.query$ = new ReplaySubject<IndexQuery>(1);
    this.response$ = new ReplaySubject<IndexResponse>(1);

    combineLatest(this.query$, filtersetSvc.filtersetNameMap).subscribe(([query, nameMap]) => {
      this.filterset = nameMap[query.filterset] || '';
    });

    combineLatest(this.query$, this.response$).subscribe(([query, response]) => {
      this.facets = this.readFacetFromResponse(query, response);
      this.queryString = query.query || '';
    });
  }


  readFacetFromResponse(request: IndexQuery, queryResponse: IndexResponse): Facet[] {
    return [
      this.convertFacet('lib', 'Lib', queryResponse.libs, request.libs || []),
      this.convertFacet('tag', 'Tag', queryResponse.tags, request.tags || []),
      this.convertFacet('param', 'Params', queryResponse.params, request.params || []),
      this.convertFacet('return', 'Returns', queryResponse.returns, request.returns || [])
    ];
  }

  convertFacet(name: string, title: string, facet: ResponseFacetValue[], checked: string[]) {
      const shown = facet
      .filter(f => {
          return f.count > 0 || checked.indexOf(f.value) != -1
      });
      return {
          name: name,
          title: title,
          options: 
              shown
          .map(f => {
              return {
                  name: name,
                  value: f.value,
                  checked: checked.indexOf(f.value) != -1,
                      label: f.value,
                  count: f.count
              };
          }),
          showControls: shown.length > 10,
          collapsed: false,
          filter: ''
      }
  }

  onSearch(f: HTMLFormElement) {
      const data = new FormData(f);
      this.query$.pipe(first())
        .subscribe(q => {
            const newQuery: IndexQuery = {
                filterset: q.filterset,
                query: data.get('query') as string || undefined,
                libs: data.getAll('lib') as string[] || undefined,
                params: data.getAll('param') as string[] || undefined,
                returns: data.getAll('return') as string[] || undefined,
                tags: data.getAll('tag') as string[] || undefined
            };
            this.queryChange.emit(newQuery);
        });
  }
}

interface Facet {
  name: string;
  title: string;
  filter: string;
  options: FacetOption[];
  showControls: boolean;
  collapsed: boolean;
}

export interface FacetOption {
  name: string;
  value: string;
  checked: boolean;
  label: string;
  count: number;
}

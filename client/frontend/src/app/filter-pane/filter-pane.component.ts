import { Subject, ReplaySubject, combineLatest, first } from 'rxjs';
import { Component, Input, Output, EventEmitter, HostListener, ElementRef, ViewChild } from '@angular/core';
import { IndexQuery, IndexResponse, ResponseFacetValue } from 'scmindex-common';
import { faMagnifyingGlass, faFolderOpen, faFolderClosed, faCircleChevronLeft, faCircleChevronRight } from '@fortawesome/free-solid-svg-icons';
import { IndexService } from '../index.service';
import { FontAwesomeModule } from '@fortawesome/angular-fontawesome';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { FacetFilterPipe } from '../facet-filter.pipe';

@Component({
  standalone: true,
  imports: [
      CommonModule,
      FormsModule,
      FontAwesomeModule,
      FacetFilterPipe
  ],
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

  @ViewChild('querytextfield')
  queryTextfield!: ElementRef;

  @HostListener('window:keydown.control./', ['$event'])
  focusQueryField(event: KeyboardEvent) {
      event.preventDefault();
      this.queryTextfield.nativeElement.focus();
  }

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

  constructor(filtersetSvc: IndexService) {
    this.query$ = new ReplaySubject<IndexQuery>(1);
    this.response$ = new ReplaySubject<IndexResponse>(1);

    combineLatest([this.query$, filtersetSvc.filtersetNameMap]).subscribe(([query, nameMap]) => {
      this.filterset = nameMap[query.filterset] || '';
    });

    combineLatest([this.query$, this.response$]).subscribe(([query, response]) => {
      this.facets = this.readFacetFromResponse(query, response);
      this.queryString = query.query || '';
    });
  }


  readFacetFromResponse(request: IndexQuery, queryResponse: IndexResponse): Facet[] {
    return [
      this.convertFacet('lib', 'Libraries', queryResponse.libs, request.libs || [], (a, b) => this.librarySorter(a, b)),
      this.convertFacet('tag', 'Tags', queryResponse.tags, request.tags || [], null),
      this.convertFacet('param', 'Parameter types', queryResponse.params, request.params || [], null),
      this.convertFacet('return', 'Return types', queryResponse.returns, request.returns || [], null)
    ];
  }

  srfiRegexp: RegExp = /^\(srfi (\d+).*\)$/;
  librarySorter(a: string, b: string): number {
      const aSrfi = a.match(this.srfiRegexp);
      const bSrfi = b.match(this.srfiRegexp);
      if (!aSrfi && !bSrfi)
          return a.localeCompare(b);
      else if (aSrfi && !bSrfi)
          return 1;
      else if (!aSrfi && bSrfi)
          return -1;
      else if (aSrfi && bSrfi) {
          return (+aSrfi[1]) - (+bSrfi[1]);
      }
      return 0;
  }

  convertFacet(name: string, title: string, facet: ResponseFacetValue[], checked: string[], sorter: ((a: string, b: string) => number) | null) {
      const shown = facet.filter(f => {
          return f.count > 0 || checked.indexOf(f.value) != -1
      });
      if (sorter) {
          shown.sort((a, b) => sorter(a.value, b.value));
      }
      return {
          name: name,
          title: title,
          options: shown.map(f => {
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

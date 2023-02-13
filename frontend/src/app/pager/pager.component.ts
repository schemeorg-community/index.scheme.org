import { Component, Input, Output, EventEmitter } from '@angular/core';
import { ReplaySubject, Observable, combineLatest, map } from 'rxjs';

@Component({
  selector: 'app-pager',
  templateUrl: './pager.component.html',
  styleUrls: ['./pager.component.scss']
})
export class PagerComponent {

  page$ = new ReplaySubject<number>(1);
  total$ = new ReplaySubject<number>(1);
  pageSize$ = new ReplaySubject<number>(1);

  pages: Observable<Page[]>;

  constructor() {
    this.pages = combineLatest(this.page$, this.total$, this.pageSize$).pipe(map(([page, total, pageSize]) => {
      const pageCount = Math.ceil(total / pageSize);
      const pages = new Set<number>();
      pages.add(1);
      pages.add(pageCount);
      for (let i = page - 2; i <= page + 2; i++) {
        if (i >= 1 && i <= pageCount) {
          pages.add(i);
        }
      }
      const pagesArr: number[] = Array.from(pages);
      pagesArr.sort((n1, n2) => n1 - n2);
      const gapPage = {
        gap: true,
        link: false,
        number: -1
      };
      return pagesArr.reduce<Page[]>((result: Page[], el: number) => {
        const p: Page = {
          gap: false,
          link: el != page,
          number: el
        };
        if (result.length && result[result.length - 1].number != el - 1) {
          return result.concat([gapPage, p]);
        } else {
          return result.concat([p]);
        }
      }, <Page[]>[]);
    }));
  }

  @Input()
  set page(v: number) {
    this.page$.next(v);
  }

  @Input()
  set total(v: number) {
    this.total$.next(v);
  }

  @Input()
  set pageSize(v: number) {
    this.pageSize$.next(v);
  }

  @Output()
  onPageClick = new EventEmitter<number>();

}


interface Page {
  gap: boolean;
  link: boolean;
  number: number;
}

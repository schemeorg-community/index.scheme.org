import { Component, HostBinding } from '@angular/core';
import { NavigationEnd, Router  } from '@angular/router';
import { filter, map, Observable, startWith, combineLatest } from 'rxjs';
import { IndexService } from './index.service';
import { faHome, faSearch, faFile, faTimes } from '@fortawesome/free-solid-svg-icons';
import { IndexErrorHandler } from './index-error-handler';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {

  navbarItems: Observable<NavbarItem[]>;

  faFile = faFile;
  faTimes = faTimes;

  constructor(filtersetSvc: IndexService, router: Router, public errorHandler: IndexErrorHandler) {
      const routeChange = router.events.pipe(
          filter((event) => event instanceof NavigationEnd),
          startWith(router)
      );
      this.navbarItems = combineLatest([filtersetSvc.filtersets, routeChange]).pipe(
          map(([filtersets, _]) => {
              return [{
                  label: 'Home',
                  icon: faHome,
                  link: '/',
                  isActive: router.isActive('/', {paths: 'exact', queryParams: 'exact', fragment: 'ignored', matrixParams: 'ignored'}),
                  items: []
              }, {
                  label: 'Search',
                  icon: faSearch,
                  isActive: router.isActive(router.parseUrl('/filterset'), {paths: 'subset', queryParams: 'subset', fragment: 'ignored', matrixParams: 'ignored'}),
                  items: filtersets.map(f => {
                      return {
                          label: f.name,
                          link: `filterset/${f.code}/search`,
                          isActive: false,
                          items: []
                      };
                  })
              }];
          }));

      this.selectedTheme = window.localStorage.getItem('theme') || 'default';
  }

  @HostBinding('class.theme-light')
  get lightTheme() {
      const prefersDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
      return this.selectedTheme == 'light' || (this.selectedTheme == 'default' && !prefersDark);
  }

  @HostBinding('class.theme-dark')
  get darkTheme() {
      const prefersDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
      return this.selectedTheme == 'dark' || (this.selectedTheme == 'default' && prefersDark);
  }

  selectedTheme: string;

  public onSelectTheme(theme: string) {
      window.localStorage.setItem('theme', theme);
  }

  public closeErrorWindow() {
      this.errorHandler.clear();
  }

}

interface NavbarItem {
    label: string;
    link?: string;
    icon?: any;
    externallink?: boolean;
    items: NavbarItem[];
    isActive: boolean;
}

import { Component, HostBinding } from '@angular/core';
import { NavigationEnd, Router  } from '@angular/router';
import { filter, map, Observable, startWith, combineLatest } from 'rxjs';
import { FiltersetsService } from './filtersets-service.service';
import { faHome, faSearch, faFile } from '@fortawesome/free-solid-svg-icons';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {

  navbarItems: Observable<NavbarItem[]>;

  faFile = faFile;

  constructor(filtersetSvc: FiltersetsService, router: Router) {
      const routeChange = router.events.pipe(
          filter((event) => event instanceof NavigationEnd),
          startWith(router)
      );
      this.navbarItems = combineLatest(filtersetSvc.filtersets, routeChange).pipe(
          map(([filtersets, _]) => {
              return [{
                  label: 'Home',
                  icon: faHome,
                  link: '/',
                  isActive: router.isActive('/', true),
                  items: []
              }, {
                  label: 'Search',
                  icon: faSearch,
                  isActive: router.isActive(router.parseUrl('/filterset'), false),
                  items: filtersets.map(f => { return {
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

}

interface NavbarItem {
    label: string;
    link?: string;
    icon?: any;
    externallink?: boolean;
    items: NavbarItem[];
    isActive: boolean;
}

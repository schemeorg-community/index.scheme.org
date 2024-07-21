import { ComponentFixture, TestBed } from '@angular/core/testing';

import { FilterPaneComponent } from './filter-pane.component';

describe('FilterPaneComponent', () => {
  let component: FilterPaneComponent;
  let fixture: ComponentFixture<FilterPaneComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ FilterPaneComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(FilterPaneComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

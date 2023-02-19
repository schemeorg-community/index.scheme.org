import { TestBed } from '@angular/core/testing';

import { FiltersetsService } from './filtersets-service.service';

describe('FiltersetsServiceService', () => {
  let service: FiltersetsService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(FiltersetsService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
